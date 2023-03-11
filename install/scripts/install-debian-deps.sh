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

  local KEY_NAME=id_25519.github

  ssh-keygen -t ed25519 -C "bwainwright28@gmail.com" -f ~/.ssh/$KEY_NAME -q -N ""

  printf "\n%s\n\n" "Copy and paste the following SSH public key into the github interface. Press enter when this is complete."
  cat ~/.ssh/$KEY_NAME.pub
  read ans

  cat ~/.ssh/$KEY_NAME.pub
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/$KEY_NAME
}
