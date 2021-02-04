#!/bin/sh

choice=$( find "$HOME/Pictures/wallpaper_presets" \( -type f -o -type l \) -printf '%f\n' | sort | dmenu -p "Wallpaper preset" -i "$@")

if [ -n "$choice" ]; then
  sh "$HOME/Pictures/wallpaper_presets/$choice"
fi
