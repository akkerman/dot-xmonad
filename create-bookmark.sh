#!/bin/sh

primary="$(xclip -o)"
clipboard="$(xclip -o -selection clipboard)"
file="$HOME/Dropbox/bookmarks.txt"

if [ -n "$primary" ] && [[ $primary =~ ^(https?|ftp|file|chrome)://[a-zA-Z0-9./?=_%+-]+$ ]]; then
  bookmark="$primary"
elif [ -n "$clipboard" ] && [[ $clipboard =~ ^(https?|ftp|file|chrome)://[a-zA-Z0-9./?=_%+-]+$ ]]; then
  bookmark="$clipboard"
else
  notify-send -t 3000 -u critical "No valid URL in clipboard"
  exit 1
fi

if grep -q "^$bookmark$" "$file"; then
  notify-send -t 3000 -u critical "Bookmark exists"
  exit 1
fi

echo "$bookmark" >> $file
cat ~/.config/qutebrowser/bookmarks/urls $file | sort | uniq | grep -E 'http|file' > ~/.config/qutebrowser/bookmarks/urls

notify-send -t 2500 "Bookmark added" "$bookmark"
