#!/bin/sh
# Select a screen region, OCR it and put the text on the clipboard.

img=$(mktemp --suffix=.png) || exit 1
trap 'rm -f "$img"' EXIT

flameshot gui --raw > "$img"
[ -s "$img" ] || exit 0

# drop form feed and trailing whitespace/blank lines; keep line endings as-is
text=$(tesseract "$img" stdout -l eng+nld 2>/dev/null | tr -d '\f')

if [ -z "$(printf '%s' "$text" | tr -d '[:space:]')" ]; then
    notify-send "copy text from screenshot" "no text found"
    exit 0
fi

printf '%s' "$text" | xclip -selection clipboard
notify-send "copy text from screenshot" "$(printf '%s' "$text" | wc -m) characters copied"
