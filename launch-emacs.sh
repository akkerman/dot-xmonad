#!/usr/bin/env bash
#

ID=0
notify () {
  echo "[COPY-HERE] notify:" $1 $2
  ID=$(notify-send --icon=emacs -u low -t 5000 -p -r $ID -a 'launch-emacs' "$1" "$2")
}

# Gracefully shutdown emacs if it is running
pgrep -u $UID -x emacs >/dev/null && /usr/bin/emacsclient -e "(kill-emacs)" 

# Wait until emacs is shutdown
pgrep -u $UID -x emacs >/dev/null && notify "wait until Emacs is shutdown"
while pgrep -u $UID -x emacs >/dev/null; do sleep 1; done

# Startup 
notify "Starting Emacs"
/usr/bin/emacs --daemon &

while ! emacsclient -e '(format  "ping")'; do
  echo "waiting..."
  sleep 1
done

notify "Emacs started!"
