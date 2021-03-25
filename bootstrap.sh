#! /usr/bin/env zsh

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
  set -s
  local command
  command="ln -s"
  if $force; then
    command+="fn"
  fi
  eval "$command $1 $2"
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
