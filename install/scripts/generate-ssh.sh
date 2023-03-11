generate_ssh() {
  local KEY_NAME=id_25519.github
  ssh-keygen -t ed25519 -C "bwainwright28@gmail.com" -f ~/.ssh/$KEY_NAME -q -N ""

  printf "\n%s\n\n" "Copy and paste the following SSH public key into the github interface. Press enter when this is complete."
  cat ~/.ssh/$KEY_NAME.pub
  read ans

  cat ~/.ssh/$KEY_NAME.pub
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/$KEY_NAME
}
