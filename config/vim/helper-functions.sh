#!/usr/bin/env bash
gvim() {
  if [ "$#" -eq 0 ]; then
    if [ "$(command gvim --serverlist)" ]; then
      command gvim --remote-expr "execute('cd $PWD')" > /dev/null
      command gvim --remote-expr "foreground()" > /dev/null
    else
      command gvim
    fi
    return
  fi
  command gvim --remote-silent "$@"
}
