source "$SCRIPTS/symlink.sh"
symlink_dotfiles() {
  printf "\nSymlinking dotfiles"
  echo "***"
  readonly dots="($DOTFILES/config/**/*.{dotfile,dotdir})"
  for dot in "${dots[@]/.\//}"; do
    link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
    symlink "$dot" "$HOME/$link_name"
  done

  if [[ $(uname) == "Darwin" ]]; then
    printf "\nSymlinking mac specific dotfiles"
    readonly dots="($DOTFILES/config/**/*.{dotfile.mac,dotdir.mac})"
    for dot in "${dots[@]/.\//}"; do
      link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
      symlink "$dot" "$HOME/$link_name"
    done
  fi

  if command -v apt > /dev/null || command -v freebsd-version > /dev/null; then
    printf "\nSymlinking linux specific dotfiles"
    readonly dots="($DOTFILES/config/**/*.{dotfile.linux,dotdir.linux})"
    for dot in "${dots[@]/.\//}"; do
      link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
      symlink "$dot" "$HOME/$link_name"
    done
  fi

  if command -v apt > /dev/null; then
    printf "\nSymlinking debian specific dotfiles"
    readonly dots="($DOTFILES/config/**/*.{dotfile.deb,dotdir.deb})"
    for dot in "${dots[@]/.\//}"; do
      link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
      symlink "$dot" "$HOME/$link_name"
    done
  fi

  if command -v freebsd-version > /dev/null; then
    printf "\nSymlinking freebsd specific dotfiles"
    readonly dots="($DOTFILES/config/**/*.{dotfile.freebsd,dotdir.freebsd})"
    for dot in "${dots[@]/.\//}"; do
      link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
      symlink "$dot" "$HOME/$link_name"
    done
  fi
}

