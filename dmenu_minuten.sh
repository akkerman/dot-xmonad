#!/bin/sh

target=$(echo "" | dmenu -p "Tijd (HH:MM)" "$@")

if [ -n "$target" ]; then
  minutes=$("$HOME/.local/bin/minuten" "$target" | awk -F': ' '/^Minuten/ {print $2}')
  if [ -n "$minutes" ]; then
    xdotool type "$minutes"
  fi
fi
