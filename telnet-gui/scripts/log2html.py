#!/usr/bin/env python3
"""
log2html.py - Convert telnet log files to terminal-styled HTML with ANSI rendering

Usage:
    python log2html.py input.log [output.html]
    python log2html.py input.log  # Creates input.html

Features:
    - Renders ANSI escape sequences as actual colors and formatting
    - Terminal-styled dark theme
    - Color-coded SEND (green) vs RECV (cyan) labels
    - Supports 16 colors, bold, underline, italic, dim
    - Self-contained HTML (no external dependencies)
"""


import sys
import os
import re
import base64
from html import escape


# Load and encode fonts for embedding
def get_font_base64(font_name):
    """Load a font file and return base64-encoded data."""
    script_dir = os.path.dirname(os.path.abspath(__file__))
    font_path = os.path.join(script_dir, "fonts", font_name)

    if not os.path.exists(font_path):
        # Font file not found, return empty string (will fall back to system fonts)
        return ""

    try:
        with open(font_path, "rb") as f:
            font_data = f.read()
            return base64.b64encode(font_data).decode("ascii")
    except Exception:
        return ""


# Telnet protocol bytes (RFC 854 and extensions)
TELNET_COMMANDS = {
    240: "SE",  # End of subnegotiation
    241: "NOP",  # No operation
    242: "DM",  # Data Mark
    243: "BRK",  # Break
    244: "IP",  # Interrupt Process
    245: "AO",  # Abort Output
    246: "AYT",  # Are You There
    247: "EC",  # Erase Character
    248: "EL",  # Erase Line
    249: "GA",  # Go Ahead
    250: "SB",  # Subnegotiation Begin
    251: "WILL",  # Will
    252: "WONT",  # Won't
    253: "DO",  # Do
    254: "DONT",  # Don't
    255: "IAC",  # Interpret As Command
}

# Telnet option codes (RFC 854 and extensions)
TELNET_OPTIONS = {
    0: "BINARY",  # Binary Transmission
    1: "ECHO",  # Echo
    2: "RECONNECT",  # Reconnection
    3: "SGA",  # Suppress Go Ahead
    4: "AMSN",  # Approx Message Size Negotiation
    5: "STATUS",  # Status
    6: "TIMING-MARK",  # Timing Mark
    7: "RCTE",  # Remote Controlled Trans and Echo
    8: "NAOL",  # Output Line Width
    9: "NAOP",  # Output Page Size
    10: "NAOCRD",  # Output Carriage-Return Disposition
    11: "NAOHTS",  # Output Horizontal Tab Stops
    12: "NAOHTD",  # Output Horizontal Tab Disposition
    13: "NAOFFD",  # Output Formfeed Disposition
    14: "NAOVTS",  # Output Vertical Tabstops
    15: "NAOVTD",  # Output Vertical Tab Disposition
    16: "NAOLFD",  # Output Linefeed Disposition
    17: "EXTEND-ASCII",  # Extended ASCII
    18: "LOGOUT",  # Logout
    19: "BYTE-MACRO",  # Byte Macro
    20: "DET",  # Data Entry Terminal
    21: "SUPDUP",  # SUPDUP
    22: "SUPDUP-OUTPUT",  # SUPDUP Output
    23: "SEND-LOCATION",  # Send Location
    24: "TERMINAL-TYPE",  # Terminal Type
    25: "EOR",  # End of Record
    26: "TACACS-UID",  # TACACS User Identification
    27: "OUTPUT-MARKING",  # Output Marking
    28: "TTYLOC",  # Terminal Location Number
    29: "REGIME-3270",  # Telnet 3270 Regime
    30: "X.3-PAD",  # X.3 PAD
    31: "NAWS",  # Negotiate About Window Size
    32: "TERMINAL-SPEED",  # Terminal Speed
    33: "REMOTE-FLOW",  # Remote Flow Control
    34: "LINEMODE",  # Linemode
    35: "X-DISPLAY-LOC",  # X Display Location
    36: "ENVIRON",  # Environment Option
    37: "AUTH",  # Authentication Option
    38: "ENCRYPT",  # Encryption Option
    39: "NEW-ENVIRON",  # New Environment Option
}

# ANSI color palette (xterm 256-color standard)
ANSI_COLORS = {
    # Standard colors (30-37, 40-47)
    "30": "#000000",  # Black
    "31": "#cd0000",  # Red
    "32": "#00cd00",  # Green
    "33": "#cdcd00",  # Yellow
    "34": "#0000ee",  # Blue
    "35": "#cd00cd",  # Magenta
    "36": "#00cdcd",  # Cyan
    "37": "#e5e5e5",  # White
    # Bright colors (90-97, 100-107)
    "90": "#7f7f7f",  # Bright Black (Gray)
    "91": "#ff0000",  # Bright Red
    "92": "#00ff00",  # Bright Green
    "93": "#ffff00",  # Bright Yellow
    "94": "#5c5cff",  # Bright Blue
    "95": "#ff00ff",  # Bright Magenta
    "96": "#00ffff",  # Bright Cyan
    "97": "#ffffff",  # Bright White
}

# Terminal color scheme
CSS_TEMPLATE = """
body {
    background-color: #0d1117;
    color: #c9d1d9;
    font-family: 'Consolas', 'Menlo', 'Monaco', 'DejaVu Sans Mono', 'Liberation Mono', 'Courier New', monospace;
    font-size: 15px;
    line-height: 1.3;
    padding: 20px;
    margin: 0;
}

.terminal {
    background-color: #161b22;
    border: 1px solid #30363d;
    border-radius: 6px;
    padding: 16px;
    max-width: 1200px;
    margin: 0 auto;
    box-shadow: 0 8px 24px rgba(0, 0, 0, 0.5);
}

.header {
    color: #58a6ff;
    font-weight: bold;
    border-bottom: 1px solid #30363d;
    padding-bottom: 10px;
    margin-bottom: 15px;
}

.session-start, .session-end {
    color: #8b949e;
    font-style: italic;
    margin: 10px 0;
}

.log-line {
    margin: 0;
    white-space: pre-wrap;
    word-wrap: break-word;
}


/* ANSI formatting classes */
.ansi-bold { font-weight: bold; }
.ansi-dim { opacity: 0.6; }
.ansi-italic { font-style: italic; }
.ansi-underline { text-decoration: underline; }
.ansi-blink { animation: blink 1s step-start infinite; }
.ansi-reverse { filter: invert(1); }
.ansi-hidden { visibility: hidden; }
.ansi-strikethrough { text-decoration: line-through; }

@keyframes blink {
    50% { opacity: 0; }
}

.escaped {
    color: #f778ba;
    font-weight: bold;
}

.iac {
    color: #ffa657;
    font-weight: bold;
}

.direction {
    font-weight: bold;
    margin-right: 4px;
}

.send {
    color: #00cd00;
}

.recv {
    color: #00cdcd;
}

.footer {
    margin-top: 20px;
    padding-top: 10px;
    border-top: 1px solid #30363d;
    color: #6e7681;
    font-size: 12px;
    text-align: center;
}
"""

HTML_TEMPLATE = """<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{title}</title>
    <style>
{css}
    </style>
</head>
<body>
    <div class="terminal">
        <div class="header">Telnet Session Log: {filename}</div>
{content}
        <div class="footer">Generated by log2html.py</div>
    </div>
</body>
</html>
"""


class ANSIParser:
    """Parse ANSI escape sequences and convert to HTML with CSS styles."""

    def __init__(self):
        self.reset_state()

    def reset_state(self):
        """Reset all formatting state."""
        self.fg_color = None
        self.bg_color = None
        self.bold = False
        self.dim = False
        self.italic = False
        self.underline = False
        self.blink = False
        self.reverse = False
        self.hidden = False
        self.strikethrough = False

    def parse_sgr(self, params):
        """Parse SGR (Select Graphic Rendition) parameters."""
        if not params:
            params = ["0"]

        i = 0
        while i < len(params):
            code = params[i]

            if code == "0":  # Reset
                self.reset_state()
            elif code == "1":  # Bold
                self.bold = True
            elif code == "2":  # Dim
                self.dim = True
            elif code == "3":  # Italic
                self.italic = True
            elif code == "4":  # Underline
                self.underline = True
            elif code == "5":  # Blink
                self.blink = True
            elif code == "7":  # Reverse
                self.reverse = True
            elif code == "8":  # Hidden
                self.hidden = True
            elif code == "9":  # Strikethrough
                self.strikethrough = True
            elif code == "22":  # Normal intensity (not bold/dim)
                self.bold = False
                self.dim = False
            elif code == "23":  # Not italic
                self.italic = False
            elif code == "24":  # Not underline
                self.underline = False
            elif code == "25":  # Not blink
                self.blink = False
            elif code == "27":  # Not reverse
                self.reverse = False
            elif code == "28":  # Not hidden
                self.hidden = False
            elif code == "29":  # Not strikethrough
                self.strikethrough = False
            elif code in (
                "30",
                "31",
                "32",
                "33",
                "34",
                "35",
                "36",
                "37",
            ):  # Foreground color
                self.fg_color = ANSI_COLORS.get(code)
            elif code == "39":  # Default foreground
                self.fg_color = None
            elif code in (
                "40",
                "41",
                "42",
                "43",
                "44",
                "45",
                "46",
                "47",
            ):  # Background color
                bg_code = str(int(code) - 10)  # Convert 40-47 to 30-37
                self.bg_color = ANSI_COLORS.get(bg_code)
            elif code == "49":  # Default background
                self.bg_color = None
            elif code in (
                "90",
                "91",
                "92",
                "93",
                "94",
                "95",
                "96",
                "97",
            ):  # Bright foreground
                self.fg_color = ANSI_COLORS.get(code)
            elif code in (
                "100",
                "101",
                "102",
                "103",
                "104",
                "105",
                "106",
                "107",
            ):  # Bright background
                bg_code = str(int(code) - 10)  # Convert 100-107 to 90-97
                self.bg_color = ANSI_COLORS.get(bg_code)
            elif code == "38":  # Extended foreground color
                # 38;5;N (256 color) or 38;2;R;G;B (RGB)
                if i + 1 < len(params):
                    if params[i + 1] == "5" and i + 2 < len(params):
                        # 256 color mode - simplified, just use standard colors
                        i += 2
                    elif params[i + 1] == "2" and i + 4 < len(params):
                        # RGB mode
                        r, g, b = params[i + 2], params[i + 3], params[i + 4]
                        self.fg_color = f"#{int(r):02x}{int(g):02x}{int(b):02x}"
                        i += 4
            elif code == "48":  # Extended background color
                if i + 1 < len(params):
                    if params[i + 1] == "5" and i + 2 < len(params):
                        i += 2
                    elif params[i + 1] == "2" and i + 4 < len(params):
                        r, g, b = params[i + 2], params[i + 3], params[i + 4]
                        self.bg_color = f"#{int(r):02x}{int(g):02x}{int(b):02x}"
                        i += 4

            i += 1

    def get_style(self):
        """Get current CSS style and classes."""
        classes = []
        styles = []

        if self.bold:
            classes.append("ansi-bold")
        if self.dim:
            classes.append("ansi-dim")
        if self.italic:
            classes.append("ansi-italic")
        if self.underline:
            classes.append("ansi-underline")
        if self.blink:
            classes.append("ansi-blink")
        if self.reverse:
            classes.append("ansi-reverse")
        if self.hidden:
            classes.append("ansi-hidden")
        if self.strikethrough:
            classes.append("ansi-strikethrough")

        if self.fg_color:
            styles.append(f"color: {self.fg_color}")
        if self.bg_color:
            styles.append(f"background-color: {self.bg_color}")

        return classes, styles

    def parse_text(self, text):
        """Parse text with ANSI codes and return HTML."""
        # Keep <IAC> markers from log - they show protocol bytes

        # Convert non-printable bytes (except ANSI ESC) to readable format
        def format_byte(b):
            code = ord(b)
            # Keep printable ASCII and tabs/newlines
            if 32 <= code < 127 or b in "\n\t":
                return b
            # Keep ANSI ESC for processing
            elif code == 0x1B:
                return b
            # Convert telnet protocol command bytes to symbolic names
            elif code in TELNET_COMMANDS:
                return f"<{TELNET_COMMANDS[code]}>"
            # Convert telnet option bytes to symbolic names
            elif code in TELNET_OPTIONS:
                return f"<{TELNET_OPTIONS[code]}>"
            # Convert all other control/high bytes to hex notation
            else:
                return f"\\x{code:02x}"

        text = "".join(format_byte(c) for c in text)

        # Now escape HTML
        text = escape(text)

        # Highlight telnet protocol symbols (IAC, WILL, WONT, DO, DONT, etc.)
        text = re.sub(
            r"&lt;([A-Z\-\.]+)&gt;", r'<span class="iac">&lt;\1&gt;</span>', text
        )

        # Highlight hex escapes
        text = re.sub(r"(\\x[0-9a-f]{2})", r'<span class="escaped">\1</span>', text)

        # Match ANSI CSI sequences: ESC [ ... m
        ansi_pattern = re.compile(r"\x1b\[([0-9;]*)m")

        result = []
        last_pos = 0

        for match in ansi_pattern.finditer(text):
            # Add text before this escape sequence
            if match.start() > last_pos:
                chunk = text[last_pos : match.start()]
                if chunk:
                    classes, styles = self.get_style()
                    if classes or styles:
                        class_str = f' class="{" ".join(classes)}"' if classes else ""
                        style_str = f' style="{"; ".join(styles)}"' if styles else ""
                        result.append(f"<span{class_str}{style_str}>{chunk}</span>")
                    else:
                        result.append(chunk)

            # Process the escape sequence
            params = match.group(1).split(";") if match.group(1) else ["0"]
            self.parse_sgr(params)

            last_pos = match.end()

        # Add remaining text
        if last_pos < len(text):
            chunk = text[last_pos:]
            if chunk:
                classes, styles = self.get_style()
                if classes or styles:
                    class_str = f' class="{" ".join(classes)}"' if classes else ""
                    style_str = f' style="{"; ".join(styles)}"' if styles else ""
                    result.append(f"<span{class_str}{style_str}>{chunk}</span>")
                else:
                    result.append(chunk)

        return "".join(result)


def unescape_log_data(data):
    """Convert logged escape sequences back to actual bytes for ANSI parsing."""

    # Convert \xNN hex escapes back to actual bytes
    def hex_replace(match):
        return chr(int(match.group(1), 16))

    data = re.sub(r"\\x([0-9a-fA-F]{2})", hex_replace, data)

    # Convert \n and \t back to actual characters
    data = data.replace("\\n", "\n")
    data = data.replace("\\t", "\t")

    # Remove \r (carriage return) - it doesn't have visual effect in HTML
    data = data.replace("\\r", "")

    return data


def parse_log_line(line):
    """Parse a telnet log line into components."""
    # Session headers/footers
    if line.startswith("==="):
        return {"type": "session", "text": line}

    # Regular log lines: [timestamp] DIRECTION: data
    match = re.match(r"\[([^\]]+)\]\s+(SEND|RECV):\s+(.*)", line)
    if match:
        timestamp, direction, data = match.groups()
        return {
            "type": "log",
            "timestamp": timestamp,
            "direction": direction,
            "data": data,
        }

    # Unknown format
    return {"type": "unknown", "text": line}


def format_log_entry(entry, parser):
    """Format a parsed log entry as HTML."""
    if entry["type"] == "session":
        css_class = "session-start" if "started" in entry["text"] else "session-end"
        return f'<div class="{css_class}">{escape(entry["text"])}</div>'

    if entry["type"] == "log":
        # Reset parser state for each line
        parser.reset_state()

        # Unescape log data and parse ANSI codes
        unescaped_data = unescape_log_data(entry["data"])

        # Remove leading and trailing newlines/carriage returns for cleaner display
        unescaped_data = unescaped_data.strip("\r\n")

        # Skip empty lines
        if not unescaped_data:
            return ""

        data_html = parser.parse_text(unescaped_data)

        # Check if this line contains telnet protocol codes
        has_telnet_codes = '<span class="iac">' in data_html

        # Add direction label at beginning if line contains telnet codes
        if has_telnet_codes:
            direction = entry["direction"]
            direction_class = "send" if direction == "SEND" else "recv"
            direction_label = (
                f'<span class="direction {direction_class}">[{direction}]</span> '
            )
            return f'<div class="log-line">{direction_label}{data_html}</div>'
        else:
            return f'<div class="log-line">{data_html}</div>'

    # Unknown format - just escape and display
    return f'<div class="log-line">{escape(entry["text"])}</div>'


def generate_css_with_fonts():
    """Generate CSS using system fonts (no embedded fonts)."""
    # Use system fonts - no embedded fonts needed
    # The CSS font-family already includes platform-specific monospace fonts
    return CSS_TEMPLATE


def convert_log_to_html(input_file, output_file):
    """Convert a telnet log file to HTML."""
    try:
        with open(input_file, "r", encoding="utf-8") as f:
            lines = f.readlines()
    except Exception as e:
        print(f"Error reading input file: {e}", file=sys.stderr)
        return False

    # Create ANSI parser
    parser = ANSIParser()

    # Parse and format each line, tracking previous entry for prompt detection
    content_lines = []
    prev_entry = None

    for line in lines:
        line = line.rstrip("\n\r")
        if not line:
            continue
        entry = parse_log_line(line)

        # Check if this is user input following a prompt
        if (
            prev_entry
            and prev_entry.get("type") == "log"
            and prev_entry.get("direction") == "RECV"
            and entry.get("type") == "log"
            and entry.get("direction") == "SEND"
            and content_lines
        ):
            # Remove closing </div> from previous line and append this inline
            if content_lines[-1].endswith("</div>"):
                content_lines[-1] = content_lines[-1][:-6]  # Remove </div>

                # Parse and add the SEND data inline
                parser.reset_state()
                unescaped_data = unescape_log_data(entry["data"])
                unescaped_data = unescaped_data.rstrip("\r\n")

                if unescaped_data:
                    data_html = parser.parse_text(unescaped_data)
                    content_lines[-1] += data_html + "</div>"
            else:
                content_lines.append(format_log_entry(entry, parser))
        else:
            content_lines.append(format_log_entry(entry, parser))

        prev_entry = entry

    # Generate HTML
    content = "".join(content_lines)
    html = HTML_TEMPLATE.format(
        title=os.path.basename(input_file),
        filename=os.path.basename(input_file),
        css=generate_css_with_fonts(),
        content=content,
    )

    # Write output
    try:
        with open(output_file, "w", encoding="utf-8") as f:
            f.write(html)
        print(f"Successfully converted {input_file} -> {output_file}")
        return True
    except Exception as e:
        print(f"Error writing output file: {e}", file=sys.stderr)
        return False


def main():
    if len(sys.argv) < 2:
        print("Usage: python log2html.py input.log [output.html]")
        print(
            "\nConverts telnet log files to terminal-styled HTML pages with ANSI rendering."
        )
        print("\nExample:")
        print("  python log2html.py session.log")
        print("  python log2html.py session.log session.html")
        sys.exit(1)

    input_file = sys.argv[1]

    # Check if input file exists
    if not os.path.exists(input_file):
        print(f"Error: Input file '{input_file}' not found", file=sys.stderr)
        sys.exit(1)

    # Determine output filename
    if len(sys.argv) >= 3:
        output_file = sys.argv[2]
    else:
        # Replace .log extension with .html, or append .html
        base, ext = os.path.splitext(input_file)
        output_file = base + ".html"

    # Convert
    success = convert_log_to_html(input_file, output_file)
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()
