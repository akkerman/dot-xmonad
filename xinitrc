#!/bin/sh 
export _JAVA_AWT_WM_NONREPARENTING=1
# export GTK_THEME=Adwaita:dark

# install from aur: gruvbox-material-gtk-theme-git gruvbox-material-icon-theme-git
export GTK_THEME=Gruvbox-Material-Dark

xmonad --recompile
# xsetroot -solid "#1d2021" # hard contrast
xsetroot -solid "#282828" # medium contrast
# xsetroot -solid "#32302f" # soft contrast

$HOME/.screenlayout/default.sh         # xrandr
$HOME/.fehbg                         # background
# $HOME/.config/compton/launch.sh &  # transparency / fade
$HOME/.config/polybar/launch.sh &    # dock / statusbar

# tray programs
/usr/bin/pasystray &      # sound
# /usr/sbin/dropbox start & # file sharing
/usr/sbin/flameshot &     # screen shots
# /usr/bin/synergy &        # share mouse & keyboard
# /usr/bin/clipit &         # clipboard history
$HOME/.config/xmonad/launch-emacs.sh &

# utils
/usr/sbin/dunst &
/usr/sbin/unclutter -idle 2 --start-hidden &
tmuxinator start org --suppress-tmux-version-warning=SUPPRESS-TMUX-VERSION-WARNING

# xmodmap -e 'keycode 66 = Control_L'
# xmodmap -e 'clear Lock'
# xmodmap -e 'add Control = Control_L'


# set right alt as modifier key for diacritics
setxkbmap -option compose:ralt

nm-applet &
exec xmonad

# vi: ft=sh:
