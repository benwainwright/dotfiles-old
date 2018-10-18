#! /usr/bin/env bash

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
    command+="fn"
  fi
  echo "Symlinking '$1' to '$2'"
  $command "$1" "$2"
}

symlink_dotfiles() {
  readonly dots=(./config/**/*.{dotfile,dotdir})
  for dot in "${dots[@]/.\//}"; do
    local link_name
    link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
    symlink "$PWD/$dot" "$HOME/$link_name"
  done
}

parse_args "$@"
symlink_dotfiles
