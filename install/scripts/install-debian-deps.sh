install_debian_deps() {
  sudo apt-get update

  sudo apt-get install -y \
    vim \
    zsh \
    exa \
    bat \
    fasd \
    fzf \
    htop \
    jq \
    nvm \
    pyenv \
    ripgrep \
    thefuck \
    silversearcher-ag \
    yarn \
    
  sudo chsh -s "$(which zsh)"
}
