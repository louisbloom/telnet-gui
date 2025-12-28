# Known Issues

## Windows: White Bar Artifact on First Window Resize

**Platform:** Windows (MSYS2 UCRT64)
**Status:** Known Issue - Platform Limitation
**Severity:** Minor Visual Artifact

### Description

On Windows, when resizing the telnet-gui window for the first time in each direction (wider or taller), a ~10px white bar may briefly appear along the edge being resized. This artifact:

- Only occurs on the **first** resize in each direction
- Does not appear on subsequent resizes in the same direction
- Does not appear if the window is resized smaller, then larger again
- Disappears immediately when the resize operation completes

### Root Cause

This is a Windows-specific SDL2 behavior where:

1. The Windows window manager expands the window during drag operations
2. SDL2's renderer back buffer may not be immediately updated to match the new window size
3. The newly exposed window area shows the default white background before our resize event handler can clear it
4. On the first resize, SDL2's internal renderer state may not be fully synchronized with the window size

### Attempted Fixes

Multiple approaches were attempted to resolve this issue:

1. **Immediate clear on resize event** - Clearing the renderer at the start of `SDL_WINDOWEVENT_RESIZED` handler
2. **Separate SIZE_CHANGED handling** - Handling `SDL_WINDOWEVENT_SIZE_CHANGED` events during drag
3. **Main loop size checking** - Proactively checking window size in the main loop before event polling
4. **Event watch callbacks** - Using `SDL_AddEventWatch` to catch resize events earlier
5. **Continuous presentation during resize** - Presenting frames continuously while resizing

None of these approaches fully resolved the issue, as the white bar appears before any of our code can execute.

### Workaround

The artifact is cosmetic only and does not affect functionality. Users can:

- Ignore the brief white bar (it disappears immediately)
- Resize the window smaller then larger again to avoid the artifact
- The artifact only appears once per direction per session

### Future Considerations

Potential solutions that were not attempted:

- Using a different SDL2 renderer backend (software vs. accelerated)
- Setting a custom window background color via Windows API
- Using SDL2's window flags to control resize behavior
- Investigating SDL2 version-specific behavior

This issue may be resolved in future SDL2 versions or with different renderer configurations.

## Windows: Delayed Detection of Server Connection Closure

**Platform:** Windows
**Status:** Known Issue - Platform Limitation
**Severity:** Minor UX Issue

### Description

On Windows, when the server closes the telnet connection while the client is idle (e.g., server idle timeout), the client does not immediately detect the closure. The connection appears active until the user attempts to send data, at which point the closure is detected and reported.

### Root Cause

This is a fundamental Windows Winsock limitation. When a server sends a TCP FIN (graceful close) or RST (reset) packet, Windows does not update the socket state for non-blocking sockets until actual I/O is attempted. Multiple detection methods were tested and all failed:

1. **`select()` with `exceptfds`** - Does not report the socket as exceptional after RST/FIN
2. **`recv()` with `MSG_PEEK`** - Returns `WSAEWOULDBLOCK` even after the connection is closed
3. **`getsockopt(SO_ERROR)`** - Returns 0 (no error) even after RST is received

Only when actual send/receive I/O is attempted does Windows report the connection error (e.g., `WSAECONNRESET` error 10054).

### Workaround

The connection closure will be detected and reported when the user next attempts to send data. The message "Connection lost" or "Connection closed" will appear at that time.

### TCP Keepalive

TCP keepalive can help prevent NAT/firewall timeouts from dropping idle connections, but does not solve the RST detection problem:

- **Helps with:** NAT routers and firewalls that drop idle connections
- **Does not help with:** Detecting server-initiated RST packets on Windows

### Impact

- Users may not immediately realize the connection has been closed by the server
- The connection state indicator may show "connected" briefly after server closes
- Detection occurs reliably when user next interacts with the connection
