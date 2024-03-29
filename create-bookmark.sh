#!/bin/sh

bookmark="$(xclip -o)"
file="$HOME/Dropbox/bookmarks.txt"


if grep -q "^$bookmark$" "$file"; then
  notify-send -t 3000 -u critical "Bookmark exists"
else
  echo "$bookmark" >> $file
  cat .config/qutebrowser/bookmarks/urls $file | sort | uniq > .config/qutebrowser/bookmarks/urls

  notify-send -t 2500 "Bookmark added" "$bookmark"
fi
