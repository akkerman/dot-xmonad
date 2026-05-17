#!/usr/bin/env bash

# List tmux sessions on the remote server 'intranet' in a *local* dmenu and
# attach to the selected one over ssh. dmenu must run locally (it needs the
# local X display); only listing and attaching happen remotely via ssh.

host=intranet

if command -v alacritty &>/dev/null; then
  TERMINAL="alacritty"
else
  TERMINAL="st"
fi

sessions=$(ssh "$host" "tmux ls -F '#{session_name}'" 2>/dev/null)

choice=$(printf '%s\n' "$sessions" | sort -u | dmenu -p "tmux@$host" -i "$@")

# Only act on an exact match with an existing remote session; otherwise do nothing.
if [ -n "$choice" ] && printf '%s\n' "$sessions" | grep -Fxq -- "$choice"; then
  "$TERMINAL" -t "$host $choice" -e ssh -t "$host" "tmux attach -t '$choice'"
fi
