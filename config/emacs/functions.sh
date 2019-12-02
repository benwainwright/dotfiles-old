#!/bin/env bash

EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
EMACSCLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient

emacs() {

  # check whether an Emacs server is already running and start daemon
  # if not
  if ! pgrep -l "^$(basename $EMACS)" > /dev/null; then
   echo "Emacs daemon not running. Starting..."
   $EMACS --daemon
  fi

  frames=$(emacsclient -a '' --eval '(frame-list)' 2>/dev/null | egrep -o '(frame)+')

  if [ "$(echo "$frames" | sed -n '$=')" -ge 2 ]; then
   $EMACSCLIENT --no-wait --tty "$@"
  else
   $EMACSCLIENT --no-wait --create-frame "$@"
  fi
}
