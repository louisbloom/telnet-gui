# Profiling Guide

This document describes how to profile telnet-gui performance, particularly for diagnosing slowness when processing telnet data.

## Overview

The profiling infrastructure consists of:

1. **C Pipeline Timing** - Measures time spent in each stage of the receive pipeline
2. **Lisp Profiler** - Measures time spent in each Lisp function
3. **Mock Telnet Server** - Replays recorded sessions for reproducible testing
4. **Automated Test Script** - Runs profiling tests with a single command

## Quick Start

```bash
# Run a profiling test
./scripts/run-profile-test.sh <logfile> <test-name> [telnet-gui-flags...]

# Example: baseline test (no extra Lisp scripts)
./scripts/run-profile-test.sh ~/telnet-logs/session.log baseline

# Example: with TinTin scripts loaded
./scripts/run-profile-test.sh ~/telnet-logs/session.log full-stack \
    -l tintin.lisp -l contrib/practice.lisp
```

Results are saved to `profile-results/<test-name>-<timestamp>.txt`.

## Profiling Flags

| Flag | Description |
|------|-------------|
| `--profile` | Enable Lisp profiler and C timing instrumentation |
| `--exit-on-disconnect` | Exit when telnet connection closes (for automated testing) |

## C Pipeline Timing

When `--profile` is enabled, the application measures time spent in each stage of the data receive pipeline:

```
telnet_receive() → telnet-input-hook → telnet-input-filter-hook → terminal_feed_data()
```

On exit, a report is printed:

```
=== C Pipeline Timing (87 RECV blocks) ===
telnet_receive:            avg 0.24ms  total 21.15ms
telnet-input-hook:         avg 0.17ms  total 17.08ms
telnet-input-filter-hook:  avg 0.01ms  total 0.90ms
terminal_feed_data:        avg 0.66ms  total 58.43ms
TOTAL:                     avg 1.07ms  total 105.46ms
```

**Interpreting results:**
- `telnet_receive` - Network I/O time (usually fast)
- `telnet-input-hook` - Lisp hooks processing received text (word collection, triggers)
- `telnet-input-filter-hook` - Lisp hooks transforming text before display
- `terminal_feed_data` - libvterm terminal emulation time

If `telnet-input-hook` dominates, the bottleneck is in Lisp code. Check the Lisp profile for details.

## Lisp Profiler

The Lisp profiler tracks time spent in each function. On exit, a report is printed:

```
=== Lisp Profile Report ===
Function                                      Calls    Time (ms)
---------------------------------------- ---------- ------------
telnet-input-hook                                 0       15.406
run-hook                                          0       15.194
collect-words-from-text                           0        1.188
...
```

**Note:** Call counts show 0 due to how the profiler aggregates data. Focus on the Time column.

### Manual Profiler Control

You can also control the profiler from Lisp:

```lisp
(profile-start)           ; Start profiling
(profile-stop)            ; Stop profiling
(profile-report)          ; Get profile data as list
(profile-reset)           ; Clear accumulated data
```

## Mock Telnet Server

The mock server replays recorded telnet sessions for reproducible testing.

```bash
python scripts/mock-telnet-server.py <logfile> [options]
```

**Options:**
- `--port PORT` - Port to listen on (default: 9999)
- `--timing MODE` - Timing mode:
  - `burst` - Send all data as fast as possible (default)
  - `original` - Replay with original timestamps
  - `fixed:N` - Fixed N millisecond delay between sends
- `--raw` - Strip IAC sequences, send only text content

**Example:**
```bash
# Replay at original speed
python scripts/mock-telnet-server.py session.log --timing original

# Burst mode for profiling (fastest)
python scripts/mock-telnet-server.py session.log --timing burst
```

## Recording Sessions

To create a log file for profiling:

1. Enable logging in telnet-gui (logs are written to `~/telnet-logs/` by default)
2. Play through the scenario you want to profile (combat, room exploration, etc.)
3. The log file will contain timestamped RECV blocks

Log format:
```
[2026-01-12T00:52:24] RECV: <escaped-data>
```

## gprof Support (Optional)

For deeper C-level profiling, build with gprof enabled:

```bash
cmake -B build -G Ninja -DENABLE_GPROF=ON
cmake --build build
./build/telnet-gui localhost 9999   # Run test
gprof ./build/telnet-gui gmon.out > gprof-output.txt
```

**Note:** gprof won't profile external libraries (SDL, libvterm), but shows time in our code calling them.

## Profiling Workflow

### Diagnosing Slowness

1. **Record the problem scenario** - Play through the slow section with logging enabled
2. **Run baseline test** - Profile without extra Lisp scripts to isolate terminal vs Lisp:
   ```bash
   ./scripts/run-profile-test.sh session.log baseline
   ```
3. **Run full-stack test** - Profile with your Lisp scripts:
   ```bash
   ./scripts/run-profile-test.sh session.log full-stack -l tintin.lisp -l contrib/practice.lisp
   ```
4. **Compare results** - If full-stack >> baseline, the bottleneck is in Lisp code
5. **Examine Lisp profile** - Identify which functions are slowest
6. **Isolate further** - Add scripts incrementally to find which one causes the slowdown

### Example Analysis

**Before optimization:**
```
telnet-input-hook:         avg 38.6ms  total 3356ms  <-- SLOW!
terminal_feed_data:        avg 0.67ms  total 58ms
```

This shows Lisp hooks are 50x slower than terminal emulation. The Lisp profile would show which functions to optimize.

**After optimization:**
```
telnet-input-hook:         avg 0.17ms  total 17ms   <-- FAST!
terminal_feed_data:        avg 0.66ms  total 65ms
```

Now Lisp hooks are faster than terminal emulation.

## Files

| File | Description |
|------|-------------|
| `scripts/mock-telnet-server.py` | Mock server for session replay |
| `scripts/run-profile-test.sh` | Automated profiling test runner |
| `profile-results/` | Directory for profiling output files |
| `src/main.c` | C timing instrumentation |
| `src/lisp.c` | Lisp profiler integration |

## Tips

- Use `burst` timing mode for profiling (fastest, most consistent)
- Run multiple tests to account for variance
- Profile both baseline and full-stack to isolate Lisp vs C bottlenecks
- Large text chunks (>2KB) are skipped by word collection to avoid lag
- The profiler adds minimal overhead (~1% typically)
