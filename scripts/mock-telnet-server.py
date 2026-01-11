#!/usr/bin/env python3
"""
Mock Telnet Server - Replays telnet log files for profiling and testing.

Usage:
    python mock-telnet-server.py <logfile> [options]

Options:
    --port PORT      Port to listen on (default: 9999)
    --timing MODE    Timing mode: burst, original, fixed:N (default: burst)
    --raw            Strip IAC sequences, send only text content

Examples:
    python mock-telnet-server.py session.log --timing burst
    python mock-telnet-server.py session.log --timing original
    python mock-telnet-server.py session.log --timing fixed:100 --raw
"""

import argparse
import re
import socket
import sys
import time
from datetime import datetime


def parse_timestamp(ts_str):
    """Parse timestamp like 2026-01-12T00:52:24"""
    return datetime.strptime(ts_str, "%Y-%m-%dT%H:%M:%S")


def unescape_log_data(data):
    """
    Unescape log data: convert \\n to newline, \\x1b to ESC, etc.
    The log format uses Python-style escapes.
    """
    result = []
    i = 0
    while i < len(data):
        if data[i] == '\\' and i + 1 < len(data):
            next_char = data[i + 1]
            if next_char == 'n':
                result.append('\n')
                i += 2
            elif next_char == 'r':
                result.append('\r')
                i += 2
            elif next_char == 't':
                result.append('\t')
                i += 2
            elif next_char == '\\':
                result.append('\\')
                i += 2
            elif next_char == 'x' and i + 3 < len(data):
                # \xNN hex escape
                try:
                    hex_val = data[i+2:i+4]
                    result.append(chr(int(hex_val, 16)))
                    i += 4
                except ValueError:
                    result.append(data[i])
                    i += 1
            else:
                result.append(data[i])
                i += 1
        else:
            result.append(data[i])
            i += 1
    return ''.join(result)


def strip_iac_sequences(data):
    """Strip telnet IAC sequences from data."""
    result = []
    i = 0
    data_bytes = data.encode('latin-1') if isinstance(data, str) else data

    while i < len(data_bytes):
        if data_bytes[i] == 0xFF:  # IAC
            if i + 1 < len(data_bytes):
                cmd = data_bytes[i + 1]
                if cmd in (0xFB, 0xFC, 0xFD, 0xFE):  # WILL, WONT, DO, DONT
                    i += 3  # Skip IAC + cmd + option
                elif cmd == 0xFF:  # Escaped 0xFF
                    result.append(0xFF)
                    i += 2
                else:
                    i += 2  # Skip IAC + cmd
            else:
                i += 1
        else:
            result.append(data_bytes[i])
            i += 1

    return bytes(result)


def parse_log_file(filepath):
    """
    Parse telnet log file and extract RECV blocks with timestamps.

    Returns list of (timestamp, data_bytes) tuples.
    """
    recv_pattern = re.compile(r'\[(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})\] RECV: (.*)$')

    entries = []

    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        for line in f:
            line = line.rstrip('\n')
            match = recv_pattern.match(line)
            if match:
                timestamp_str = match.group(1)
                data_escaped = match.group(2)

                timestamp = parse_timestamp(timestamp_str)
                data = unescape_log_data(data_escaped)
                data_bytes = data.encode('latin-1', errors='replace')

                entries.append((timestamp, data_bytes))

    return entries


def run_server(entries, port, timing_mode, raw_mode):
    """
    Run the mock telnet server.

    Single-client mode: accepts one connection, sends all data, exits.
    """
    # Parse timing mode
    if timing_mode == 'burst':
        delay_fn = lambda prev_ts, curr_ts: 0
    elif timing_mode == 'original':
        delay_fn = lambda prev_ts, curr_ts: (curr_ts - prev_ts).total_seconds() if prev_ts else 0
    elif timing_mode.startswith('fixed:'):
        fixed_ms = int(timing_mode.split(':')[1])
        delay_fn = lambda prev_ts, curr_ts: fixed_ms / 1000.0
    else:
        print(f"Unknown timing mode: {timing_mode}", file=sys.stderr)
        sys.exit(1)

    # Create server socket
    server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind(('0.0.0.0', port))
    server.listen(1)

    print(f"Mock telnet server listening on port {port}")
    print(f"Timing mode: {timing_mode}, Raw mode: {raw_mode}")
    print(f"Loaded {len(entries)} RECV blocks from log")
    print("Waiting for client connection...")

    client, addr = server.accept()
    print(f"Client connected from {addr}")

    total_bytes = 0
    prev_ts = None

    try:
        for i, (timestamp, data) in enumerate(entries):
            # Apply timing delay
            delay = delay_fn(prev_ts, timestamp)
            if delay > 0:
                time.sleep(delay)
            prev_ts = timestamp

            # Process data
            if raw_mode:
                send_data = strip_iac_sequences(data)
            else:
                send_data = data

            # Send to client
            try:
                client.sendall(send_data)
                total_bytes += len(send_data)
            except (BrokenPipeError, ConnectionResetError):
                print(f"Client disconnected after {i} blocks")
                break

        print(f"Sent {len(entries)} blocks, {total_bytes} bytes total")

    finally:
        # Give client time to read final data
        time.sleep(0.5)
        client.close()
        server.close()

    print("Server finished")


def main():
    parser = argparse.ArgumentParser(
        description='Mock Telnet Server - Replays telnet log files',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__
    )
    parser.add_argument('logfile', help='Telnet log file to replay')
    parser.add_argument('--port', type=int, default=9999, help='Port to listen on (default: 9999)')
    parser.add_argument('--timing', default='burst',
                        help='Timing mode: burst, original, fixed:N (default: burst)')
    parser.add_argument('--raw', action='store_true',
                        help='Strip IAC sequences, send only text content')

    args = parser.parse_args()

    # Parse log file
    print(f"Parsing log file: {args.logfile}")
    entries = parse_log_file(args.logfile)

    if not entries:
        print("No RECV entries found in log file", file=sys.stderr)
        sys.exit(1)

    # Run server
    run_server(entries, args.port, args.timing, args.raw)


if __name__ == '__main__':
    main()
