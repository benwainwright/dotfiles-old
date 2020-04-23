#!/usr/bin/env bash

e() {

  # check whether an Emacs server is already running and start daemon
  # if not
  if ! pgrep -l "Emacs" > /dev/null; then
   echo "Emacs daemon not running. Starting..."
   emacs --daemon
  fi

  frames=$(emacsclient -a '' --eval '(frame-list)' 2>/dev/null | egrep -o '(frame)+')

  if [ "$(echo "$frames" | sed -n '$=')" -ge 2 ]; then
   emacsclient --no-wait --tty "$@"
  else
   emacsclient --no-wait --create-frame "$@"
  fi
}
