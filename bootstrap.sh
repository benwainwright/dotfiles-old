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
    command+="f"
  fi
  echo "Symlinking '$1' to '$2'"
  $command "$1" "$2"
}

symlink_dotfiles() {
  readonly dotfiles=$(   \
    find ./config \( -name "*.dotfile" -o -name "*.dotdir" \) \
    -mindepth 2 \
    -maxdepth 2 | sed "s|^\\./||")

  for dotfile in $dotfiles; do
    local link_name
    link_name=."$(echo "$dotfile" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
    symlink "$PWD/$dotfile" "$HOME/$link_name"
  done
}

parse_args "$@"
symlink_dotfiles
