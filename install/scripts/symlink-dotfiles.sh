source "$SCRIPTS/symlink.sh"
symlink_dotfiles() {
  setopt extended_glob
  setopt NULL_GLOB

  printf "\n\nSymlinking dotfiles\n***\n\n"

  readonly dots=($DOTFILES/config/**/*.{dotfile,dotdir})
  for dot in "${dots[@]/.\//}"; do
    link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
    symlink "$dot" "$HOME/$link_name"
  done

  if [[ $(uname) == "Darwin" ]]; then
    printf "\n\nSymlinking mac specific dotfiles\n***\n\n"
    readonly dots=($DOTFILES/config/**/*.{dotfile.mac,dotdir.mac})
    for dot in "${dots[@]/.\//}"; do
      link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
      symlink "$dot" "$HOME/$link_name"
    done
  fi

  if command -v apt > /dev/null || command -v freebsd-version > /dev/null; then
    printf "\n\nSymlinking linux specific dotfiles\n***\n\n"
    readonly dots=($DOTFILES/config/**/*.{dotfile.linux,dotdir.linux})
    for dot in "${dots[@]/.\//}"; do
      link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
      symlink "$dot" "$HOME/$link_name"
    done
  fi

  if command -v apt > /dev/null; then
    printf "\n\nSymlinking debian specific dotfiles\n***\n\n"
    readonly dots=($DOTFILES/config/**/*.{dotfile.deb,dotdir.deb})
    for dot in "${dots[@]/.\//}"; do
      link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
      symlink "$dot" "$HOME/$link_name"
    done
  fi

  if command -v freebsd-version > /dev/null; then
    printf "\nSymlinking freebsd specific dotfiles"
    readonly dots=($DOTFILES/config/**/*.{dotfile.freebsd,dotdir.freebsd})
    for dot in "${dots[@]/.\//}"; do
      link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
      symlink "$dot" "$HOME/$link_name"
    done
  fi
}

