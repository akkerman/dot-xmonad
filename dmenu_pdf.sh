#!/bin/sh
choice=$(find "$HOME/Dropbox/Apps" "$HOME/Dropbox/Library Bought" "$HOME/Dropbox/Library" "$HOME/Documents" -name "*.pdf"  -o -name "*.epub" | dmenu -l 20 -i "$@")

if [ -n "$choice" ]; then
  evince "$choice"
fi

