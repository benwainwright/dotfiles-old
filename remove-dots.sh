#!/usr/bin/env zsh

SCRIPTS=$DOTFILES/install/scripts
DOTFILES=~/dotfiles

source $SCRIPTS/index.sh

setopt extended_glob
setopt +o nullglob

printf "\n\nRemoving dotfiles\n***"

readonly dots=($DOTFILES/config/**/*.{dotfile,dotdir})
for dot in "${dots[@]/.\//}"; do
  link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
  echo "removing $link_name"
  rm "$HOME/$link_name"
done

if [[ $(uname) == "Darwin" ]]; then
  printf "\n\nRemoving mac specific symlinks\n***"
  readonly dots=($DOTFILES/config/**/*.{dotfile.mac,dotdir.mac})
  for dot in "${dots[@]/.\//}"; do
    link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
    echo "removing $link_name"
    rm "$HOME/$link_name"
  done
fi

if command -v apt > /dev/null || command -v freebsd-version > /dev/null; then
  printf "\n\nRemoving linux specific symlinks\n***"
  readonly dots=($DOTFILES/config/**/*.{dotfile.linux,dotdir.linux})
  for dot in "${dots[@]/.\//}"; do
    link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
    echo "removing $link_name"
    rm "$HOME/$link_name"
  done
fi

if command -v apt > /dev/null; then
  printf "\n\n Removing debian specific symlinks\n***"
  readonly dots=($DOTFILES/config/**/*.{dotfile.deb,dotdir.deb})
  for dot in "${dots[@]/.\//}"; do
    link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
    echo "removing $link_name"
    rm "$HOME/$link_name"
  done
fi

if command -v freebsd-version > /dev/null; then
  printf "\n\n Removing freebsd specific dotfiles"
  readonly dots=($DOTFILES/config/**/*.{dotfile.freebsd,dotdir.freebsd})
  for dot in "${dots[@]/.\//}"; do
    link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
    echo "removing $link_name"
    rm "$HOME/$link_name"
  done
fi

