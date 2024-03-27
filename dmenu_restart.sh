#!/bin/sh


choice=$(echo -e "commit slip-box\nrestart status bar\nreload wallpaper\nrandom wallpaper\nremove current wallpaper\nrestart emacs daemon\nenable transparancy\ndisable transparancy" | dmenu -i "$@")

[ "$choice" = "commit slip-box" ] && systemctl --user restart commit-slipbox.service
[ "$choice" = "restart status bar" ] && $HOME/.config/polybar/launch.sh
[ "$choice" = "enable transparancy" ] && $HOME/.config/compton/launch.sh
[ "$choice" = "disable transparancy" ] && killall compton
[ "$choice" = "reload wallpaper" ] && sh $HOME/.fehbg
[ "$choice" = "random wallpaper" ] && $HOME/Pictures/wallpapers/one_random_background.sh
[ "$choice" = "remove current wallpaper" ] && $HOME/Pictures/wallpapers/remove_current_background.sh && $HOME/Pictures/wallpapers/one_random_background.sh
[ "$choice" = "restart emacs daemon" ] && $HOME/.config/xmonad/launch-emacs.sh

