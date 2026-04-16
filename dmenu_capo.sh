#!/bin/sh

cd "$HOME/git/dsplatform/capo-frontend"
choice=$(ls -d */ | sed 's:/$::' | dmenu -l 20 -i "$@")


if [ -n "$choice" ]; then
  tmuxinator stop capo && true
  tmuxinator stop capo-e2e && true
  
  st -t "mux CAPO $choice" -e env WORKTREE="$choice" tmuxinator capo
fi
