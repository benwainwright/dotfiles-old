#! /usr/bin/env zsh

DOTFILES=~/dotfiles
SCRIPTS=$DOTFILES/install/scripts

source $SCRIPTS/index.sh

parse_args "$@"
symlink_dotfiles

if [[ $(uname) == "Darwin" ]]; then
    copy_fonts
    install_brew_dependencies
    set_macos_defaults
    set_iterm_preferences_location
elif command -v apt > /dev/null; then
    install_debian_deps
fi


if [ $python ] || [ $all ]; then
  install_python_3
fi
