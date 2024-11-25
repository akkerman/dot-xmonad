#!/bin/bash

# Log file location
LOG_FILE="$HOME/.local/state/dmenu_usage.log"

# Run dmenu_run and get the selected command
# SELECTED=$(dmenu_run -i "$@") # doesn't work, starts the program instead of returning it

SELECTED=$(compgen -c | sort -u | dmenu -i -p "Run: " "$@")

# If a command is selected, log it and execute it
if [[ -n "$SELECTED" ]]; then
    echo "$SELECTED" >> "$LOG_FILE"
    eval "$SELECTED" &
fi
