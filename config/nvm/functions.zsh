nvm() {
  # Sourcing NVM into the shell takes a crazy amount of time, so this little script, 
  # Borrowed from https://medium.com/@dannysmith/little-thing-2-speeding-up-zsh-f1860390f92
  # Sets up lazy loading
  echo "NVM not yet loaded! Loading now..."
  unset -f nvm
  export NVM_DIR="$HOME/.nvm"
  export NVM_PREFIX=$(brew --prefix nvm)
  if [ -s "$NVM_PREFIX/nvm.sh" ]; then
    source "$NVM_PREFIX/nvm.sh"
  fi
  nvm "$@"
}
