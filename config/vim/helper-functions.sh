#!/usr/bin/env bash
gvim() {

  if [ "$#" -eq 0 ] && [ "$(gvim --serverlist)" ]; then
    command gvim --remote-expr "foreground()" > /dev/null
    return
  fi

  command gvim --remote-silent "$@"
}
