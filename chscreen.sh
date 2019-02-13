#!/bin/sh

choice=$( find "$HOME/.screenlayout" \( -type f -o -type l \) -printf '%f\n' | sort | dmenu -i "$@")

if [ -n "$choice" ]; then
  sh "$HOME/.screenlayout/$choice"
  xmonad --restart
  "$HOME/.config/polybar/launch.sh"


  killall synergys
  killall synergy 

  while pgrep -u $UID -x synergy >/dev/null; do sleep 1; done
  synergy &
fi
