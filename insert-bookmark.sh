#!/bin/sh

choice=$(grep -v '^#' $HOME/Dropbox/bookmarks.txt | sort | dmenu -p "Insert bookmark" -i -l 20 "$@")

if [ -n "$choice" ]; then
  xdotool type "$choice"
fi
