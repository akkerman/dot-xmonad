#!/bin/sh

choice=$(echo -e "polybar\ncompton" | dmenu -i "$@")

[ "$choice" = "polybar" ] && $HOME/.config/polybar/launch.sh
[ "$choice" = "compton" ] && $HOME/.config/compton/launch.sh
