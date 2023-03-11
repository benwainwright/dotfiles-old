install_debian_deps() {
  sudo apt-get update

  sudo apt-get install -y \
    vim \
    zsh \
    exa \
    fasd \
    fzf \
    htop \
    jq \
    ripgrep \
    thefuck \
    silversearcher-ag \
    yarn \
    
  sudo chsh -s "$(which zsh)"
}
