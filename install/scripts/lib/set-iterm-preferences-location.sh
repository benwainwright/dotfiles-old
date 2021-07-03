set_iterm_preferences_location() {
  echo "\nConfiguring iTerm2 to use your saved preferences file"
  defaults write com.googlecode.iterm2 PrefsCustomFolder -string "$HOME/dotfiles/config/iterm"
}
