#!/bin/sh

choice=$(grep -v '^#' $HOME/Dropbox/bookmarks.txt | sort | dmenu -p "Select Bookmark" -i -l 20 "$@")

if [ -n "$choice" ]; then
 qutebrowser --target window "$choice"
fi
