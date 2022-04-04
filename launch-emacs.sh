#!/usr/bin/env bash

# Gracefully shutdown emacs if it is running
pgrep -u $UID -x emacs >/dev/null && /usr/bin/emacsclient -e "(kill-emacs)" 

# Wait until emacs is shutdown
while pgrep -u $UID -x emacs >/dev/null; do sleep 1; done

# Startup 
/usr/bin/emacs --daemon &

notify-send "Emacs started"
