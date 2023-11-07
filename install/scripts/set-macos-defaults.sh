set_macos_defaults() {
  if [[ $OSTYPE == 'darwin'* ]]; then
    echo "\nConfiguring sensible Mac defaults"
    # Close any open System Preferences panes, to prevent them from overriding
    # settings we’re about to change
    osascript -e 'tell application "System Preferences" to quit'

    # Ask for the administrator password upfront
    sudo -v

    # Keep-alive: update existing `sudo` time stamp until `.macos` has finished
    while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

    # disable the sound effects on boot
    sudo nvram SystemAudioVolume=" "

    defaults write NSGlobalDomain AppleShowAllExtensions -bool true

    # Expand save / print panels by default
    defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
    defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true
    defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
    defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

    # Unhide library folder
    chflags nohidden ~/Library

    # Disable the “Are you sure you want to open this application?” dialog
    defaults write com.apple.LaunchServices LSQuarantine -bool false

    # Allows the VSCode neovim plugin to interpret press and hold as a repeated keypress
    defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool falsedefaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false

    # Remove duplicates in the “Open With” menu (also see `lscleanup` alias)
    /System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user

    # Reveal IP address, hostname, OS version, etc. when clicking the clock
    # in the login window
    sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

    # Disable the crash reporter
    defaults write com.apple.CrashReporter DialogType -string "none"

    # Reveal IP address, hostname, OS version, etc. when clicking the clock
    # in the login window
    sudo defaults write /Library/Preferences/com.apple.loginwindow AdminHostInfo HostName

  else
    echo "Not installing on a Mac, no need to set MacOS defaults"
  fi
}
