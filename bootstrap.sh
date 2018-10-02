#! /usr/bin/env bash

set -x

force=false

parse_args() {
    while (( "$#" )); do
      case "$1" in
        --force)
          force=true
          shift
      esac
    done
}

symlink() {
  local command
  command="ln -s"
  if $force; then
    command+="f"
    echo "Symlinking '$1' to '$2'"
  fi
  $command "$1" "$2"
}

symlink_dotfiles() {
  readonly dotfiles=$(find . -name "*.dotfile" -mindepth 2 | sed "s|^\\./||")

  for dotfile in $dotfiles; do
    local link_name
    link_name=."$(echo "$dotfile" | xargs basename | cut -d. -f1)"
    symlink "$PWD/$dotfile" "$HOME/$link_name"
  done
}

parse_args "$@"
symlink_dotfiles

