#! /usr/bin/env zsh

DOTFILES=~/dotfiles
SCRIPTS=$DOTFILES/install/scripts

source $SCRIPTS/lib/index.sh

parse_args "$@"

if [ $symlink ] || [ $all ]; then
  symlink_dotfiles
fi

if [ $fonts ] || [ $all ]; then
  copy_fonts
fi

if [ $brew ] || [ $all ]; then
  install_brew_dependencies
fi

if [ $defaults ] || [ $all ]; then
  set_macos_defaults
fi

if [ $python ] || [ $all ]; then
  install_python_3
fi

if [ $iterm ] || [ $all ]; then
  set_iterm_preferences_location
fi
