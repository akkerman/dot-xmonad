#!/bin/sh

choice=$( find "$HOME/Pictures/wallpaper_presets" \( -type f -o -type l \) -printf '%f\n' | sort | dmenu -p "Wallpaper preset" -i "$@")

if [ -n "$choice" ]; then
  cp "$HOME/Pictures/wallpaper_presets/$choice" ~/.fehbg
  sh ~/.fehbg
fi
