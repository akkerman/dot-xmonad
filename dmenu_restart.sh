#!/bin/sh

choice=$(echo -e "restart status bar\nenable transparancy\ndisable transparancy\nreload wallpaper\nrandom wallpaper\nremove current wallpaper" | dmenu -i "$@")

[ "$choice" = "restart status bar" ] && $HOME/.config/polybar/launch.sh
[ "$choice" = "enable transparancy" ] && $HOME/.config/compton/launch.sh
[ "$choice" = "disable transparancy" ] && killall compton
[ "$choice" = "reload wallpaper" ] && sh $HOME/.fehbg
[ "$choice" = "random wallpaper" ] && $HOME/Pictures/wallpapers/one_random_background.sh
[ "$choice" = "remove current wallpaper" ] && $HOME/Pictures/wallpapers/remove_current_background.sh && $HOME/Pictures/wallpapers/one_random_background.sh

