#!/bin/bash
# generate_png.sh - Generate PNG images from plotcli terminal output
#
# This script captures ANSI-colored terminal output from R/plotcli and converts
# it to PNG images with a nice terminal theme (Solarized Dark).
#
# Requirements:
#   - R with plotcli, ggplot2, dplyr installed
#   - Python 3 with ansi2html: pip install ansi2html
#   - Chromium or Google Chrome browser
#
# Usage:
#   ./generate_png.sh input.R output.png [width] [height]
#
# Example:
#   # Create a simple R script
#   echo 'library(plotcli); library(ggplot2)
#   p <- ggplot(mtcars, aes(x=wt, y=mpg, color=factor(cyl))) + geom_point()
#   ggplotcli(p, width=70, height=18)' > myplot.R
#
#   # Generate PNG
#   ./generate_png.sh myplot.R myplot.png
#
#   # With custom dimensions (default: 900x550)
#   ./generate_png.sh myplot.R myplot.png 1000 600

set -e

# Check arguments
if [ $# -lt 2 ]; then
    echo "Usage: $0 <input.R> <output.png> [width] [height]"
    echo ""
    echo "Example:"
    echo "  $0 myplot.R myplot.png"
    echo "  $0 myplot.R myplot.png 1000 600"
    exit 1
fi

INPUT_R="$1"
OUTPUT_PNG="$2"
WIDTH="${3:-900}"
HEIGHT="${4:-550}"

# Check if input file exists
if [ ! -f "$INPUT_R" ]; then
    echo "Error: Input file '$INPUT_R' not found"
    exit 1
fi

# Check dependencies
if ! command -v Rscript &> /dev/null; then
    echo "Error: Rscript not found. Please install R."
    exit 1
fi

if ! python3 -c "import ansi2html" &> /dev/null; then
    echo "Error: ansi2html not found. Install with: pip install ansi2html"
    exit 1
fi

# Find chromium or chrome
CHROME=""
for cmd in chromium chromium-browser google-chrome google-chrome-stable; do
    if command -v "$cmd" &> /dev/null; then
        CHROME="$cmd"
        break
    fi
done

if [ -z "$CHROME" ]; then
    echo "Error: Chromium or Google Chrome not found"
    exit 1
fi

# Create temp files
TEMP_DIR=$(mktemp -d)
TEMP_HTML="$TEMP_DIR/plot.html"
TEMP_ANSI="$TEMP_DIR/plot.ansi"

# Cleanup on exit
cleanup() {
    rm -rf "$TEMP_DIR"
}
trap cleanup EXIT

# Run R script and capture ANSI output
echo "Running R script..."
LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 Rscript "$INPUT_R" 2>/dev/null > "$TEMP_ANSI"

# Convert ANSI to HTML with inline styles
echo "Converting to HTML..."
ANSI_CONTENT=$(cat "$TEMP_ANSI" | python3 -m ansi2html --inline)

# Create themed HTML
cat > "$TEMP_HTML" << EOF
<!DOCTYPE html>
<html>
<head>
<style>
* { margin: 0; padding: 0; box-sizing: border-box; }
html, body {
  background: #073642;
}
.terminal {
  background: #073642;
  padding: 10px 15px;
  font-family: "DejaVu Sans Mono", "Liberation Mono", "Consolas", monospace;
  font-size: 13px;
  line-height: 1.35;
  color: #839496;
  white-space: pre;
}
/* Solarized Dark ANSI color overrides */
.terminal span[style*="color: #aa0000"] { color: #dc322f !important; }
.terminal span[style*="color: #00aa00"] { color: #859900 !important; }
.terminal span[style*="color: #aa5500"] { color: #b58900 !important; }
.terminal span[style*="color: #0000aa"] { color: #268bd2 !important; }
.terminal span[style*="color: #aa00aa"] { color: #d33682 !important; }
.terminal span[style*="color: #00aaaa"] { color: #2aa198 !important; }
.terminal span[style*="color: #E850A8"] { color: #6c71c4 !important; }
</style>
</head>
<body>
<div class="terminal">
$ANSI_CONTENT
</div>
</body>
</html>
EOF

# Convert HTML to PNG using headless Chrome
echo "Generating PNG..."
"$CHROME" --headless --disable-gpu --screenshot="$OUTPUT_PNG" \
    --window-size="${WIDTH},${HEIGHT}" \
    "file://$TEMP_HTML" 2>/dev/null

echo "Done! Output saved to: $OUTPUT_PNG"
