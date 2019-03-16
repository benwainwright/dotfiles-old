alias bi='brew install'
alias  brew_list_formula='find /usr/local/Homebrew/ -type d -name "Formula" -exec ls -1 {} \; | cut -d"." -f1'
alias blf='brew_list_formula'
alias brew_list_formula_fzf="brew_list_formula | fzf --height=40% --preview='brew info {}'"
alias blff='brew_list_formula_fzf'
alias bif='brew install $(blff)'


