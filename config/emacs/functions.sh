#!/bin/env bash

EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
EMACSCLIENT=/Applications/Emacs.app/Contents/MacOS/bin/emacsclient

emacs() {
  # Borrowed from https://stackoverflow.com/questions/25029877/using-emacs-as-a-server-and-opening-only-one-window-exactly-which-should-be-max

  # Selected options for "emacsclient"

  # -c          Create a new frame instead of trying to use the current
  #             Emacs frame.

  # -e          Evaluate the FILE arguments as ELisp expressions.

  # -n          Don't wait for the server to return.

  # -t          Open a new Emacs frame on the current terminal.

  # Note that the "-t" and "-n" options are contradictory: "-t" says to
  # take control of the current text terminal to create a new client frame,
  # while "-n" says not to take control of the text terminal.  If you
  # supply both options, Emacs visits the specified files(s) in an existing
  # frame rather than a new client frame, negating the effect of "-t".

  # check whether an Emacs server is already running and start daemon
  # if not
  if ! pgrep -l "^Emacs" > /dev/null; then
   echo "Emacs daemon not running. Starting..."
   $EMACS --daemon
  fi

  # return a list of all frames on $DISPLAY open in current
  # frame if frames detected, or in new frame if not
  if emacsclient -e "(frames-on-display-list \"$DISPLAY\")" &>/dev/null; then
   $EMACSCLIENT -n -t "$@"
  else
   $EMACSCLIENT -n -c "$@"
  fi
}
