#!/bin/sh
choice=$(find "$HOME/Dropbox/Apps" "$HOME/Dropbox/Library Bought" "$HOME/Dropbox/Library" "$HOME/Documents" "$HOME/git/dsplatform/analytics-wiki" -name "*.pdf"  -o -name "*.epub" -o -name "*.mobi" -o -name "*.md" | dmenu -l 20 -i "$@")

msg="opened nothing"



if [ -n "$choice" ]; then
  filename=$(basename -- "$choice")
  extension="${filename##*.}"
  filename="${filename%.*}"
  case $extension in
    "pdf")
      exec zathura "$choice"
      ;;
    "mobi")
      exec ebook-viewer "$choice"
      ;;
    "epub")
      exec ebook-viewer "$choice"
      ;;
    "md")
      exec st -e nvim "$choice" +MarkdownPreview
      notify-send "opening $choice"
      ;;
  esac
fi
