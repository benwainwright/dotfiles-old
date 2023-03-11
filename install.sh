#! /usr/bin/env zsh

DOTFILES=~/dotfiles
SCRIPTS=$DOTFILES/install/scripts

source $SCRIPTS/index.sh


if [[ $(uname) == "Darwin" ]]; then
    copy_fonts
    install_brew_dependencies
    set_macos_defaults
    set_iterm_preferences_location
fi

if command -v apt > /dev/null || command -v freebsd-version > /dev/null; then
    install_linux_deps
fi

if command -v apt > /dev/null; then
    install_debian_deps
fi

if command -v freebsd-version > /dev/null; then
    install_freebsd_deps
fi

symlink_dotfiles

if [ $python ] || [ $all ]; then
    install_python_3
fi
