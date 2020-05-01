#!/bin/sh

choice=$(echo -e "restart status bar\nenable transparancy\ndisable transparancy" | dmenu -i "$@")

[ "$choice" = "restart status bar" ] && $HOME/.config/polybar/launch.sh
[ "$choice" = "enable transparancy" ] && $HOME/.config/compton/launch.sh
[ "$choice" = "disable transparancy" ] && killall compton

