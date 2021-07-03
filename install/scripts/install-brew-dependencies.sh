install_brew_dependencies() {
  echo "\nChecking for Homebrew"
  if ! type brew > /dev/null; then
    echo "Installing Homebrew"
    # Recommended way of installing homebrew from their website
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  else
    echo "Homebrew is already installed!"
  fi

  echo "\nInstalling from Brewfile"
  brew bundle --file $DOTFILES/install/Brewfile
}
