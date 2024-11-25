#!/bin/sh

bookmark="$(xclip -o)"
file="$HOME/Dropbox/bookmarks.txt"

if ! [[ $bookmark =~ ^(https?|ftp|file|chrome)://[a-zA-Z0-9./?=_%+-]+$ ]]; then
  notify-send -t 3000 -u critical "Invalid URL" "$bookmark"
  exit 1
fi

if grep -q "^$bookmark$" "$file"; then
  notify-send -t 3000 -u critical "Bookmark exists"
  exit 1
fi

echo "$bookmark" >> $file
cat ~/.config/qutebrowser/bookmarks/urls $file | sort | uniq | grep -E 'http|file' > ~/.config/qutebrowser/bookmarks/urls

notify-send -t 2500 "Bookmark added" "$bookmark"
