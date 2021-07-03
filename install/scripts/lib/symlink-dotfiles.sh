source $SCRIPTS/lib/symlink.sh
symlink_dotfiles() {
  echo "\nSymlinking dotfiles"
  echo "***"
  readonly dots=($DOTFILES/config/**/*.{dotfile,dotdir})
  for dot in "${dots[@]/.\//}"; do
    link_name=."$(echo "$dot" | xargs basename -s .dotfile | xargs basename -s .dotdir)"
    symlink "$dot" "$HOME/$link_name"
  done
}

