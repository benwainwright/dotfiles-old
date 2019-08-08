alias ls='exa --color=always'
alias l='ls -lFh'     #size,show type,human readable
alias lr='ls -tRFh'   #sorted by date,recursive,show type,human readable
alias ldot='ls -ld .*'
alias ..='cd ..'
alias m='make'
alias cat='bat'
alias c='bat'
alias ping='prettyping'
alias ip="dig +short myip.opendns.com @resolver1.opendns.com"
alias lop='listening-on-port'
alias path='echo -e ${PATH//:/\\n}'
alias r='echo "Reloading shell" && source ~/.zshrc'
alias jf='cd $(fasd -ld | fzf)'

