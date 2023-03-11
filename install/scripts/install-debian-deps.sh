install_debian_deps() {
  sudo apt-get update

  sudo apt-get install \
    vim \
    zsh

  sudo chsh -s "$(which zsh)"
}
