#!/usr/bin/env bash
mvim-remote() {
  if [ "$#" -eq 0 ]; then
    if [ "$(command mvim --serverlist)" ]; then
      command mvim --remote-expr "execute('cd $PWD')" > /dev/null
      command mvim --remote-expr "foreground()" > /dev/null
    else
      command mvim
    fi
    return
  fi
  command mvim --remote-silent "$@"
}
