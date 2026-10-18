#!/usr/bin/env bash
#

ID=0
notify () {
  echo "[LAUNCH EMACS] notify:" "$1" "$2"
  ID=$(notify-send --icon=emacs -u low -t 5000 -p -r "$ID" -a 'launch-emacs' "$1" "$2")
}

# Gracefully shutdown emacs if it is running
pgrep -u $UID -x emacs >/dev/null && /usr/bin/emacsclient -e "(kill-emacs)" 

# Wait until emacs is shutdown
pgrep -u $UID -x emacs >/dev/null && notify "wait until Emacs is shutdown"
while pgrep -u $UID -x emacs >/dev/null; do sleep 1; done

# Startup 
notify "Starting Emacs"
/usr/bin/emacs --daemon &
EMACS_PID=$!

TIMEOUT=60
START=$SECONDS
while ! emacsclient -e '(format  "ping")' >/dev/null 2>&1; do
  # The launcher process exits when the daemon is up (0) or has failed (non-zero)
  if ! kill -0 "$EMACS_PID" 2>/dev/null; then
    wait "$EMACS_PID"
    STATUS=$?
    if [ "$STATUS" -ne 0 ]; then
      notify "Emacs failed to start" "emacs --daemon exited with status $STATUS"
      exit "$STATUS"
    fi
  fi
  if (( SECONDS - START >= TIMEOUT )); then
    notify "Emacs failed to start" "no response after ${TIMEOUT}s"
    exit 1
  fi
  echo "waiting..."
  sleep 1
done

notify "Emacs started!"
