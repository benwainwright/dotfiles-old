# My Dotfiles
Since other people are doing it, I thought I'd organise and share mine as well. The author bears no responsibility for any loss or damage caused to your productivity should you choose to use my dotfiles...

## Linking
Files are organised loosely into a tool based folders within the `config` directory. When you run `./bootstrap.sh` any `.dotfile` or `.dotdir` within these directories will be symlinked into the home directory.

## Shell Sourcing
Sourced files follow the normal zsh loading path initially. My zshrc then sources files in the following order:

- `~/.env.private.zsh` if it exists
- Any `.zsh` files in `dotfiles/config/zshell/init`
- Any `.zsh` files found in any of the `dotfiles/config` subdirectories

This approach avoids a huge list of aliases and functions and allows me to organise aliases, functions and shell variables around the associated tool. For example, all aliases related to git can be found in `dotfiles/config/git/aliases.zsh`
