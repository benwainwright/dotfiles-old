install_linux_deps() {
  sudo curl -sfL git.io/antibody | sudo sh -s - -b /usr/local/bin
  curl https://pyenv.run | bash
  wget -qO- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.3/install.sh | bash
}
