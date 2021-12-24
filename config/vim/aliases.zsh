alias gvim='mvim-remote'
alias gv='gvim'

# TERM entry below is to enable undercurl. See https://wezfurlong.org/wezterm/faq.html#how-do-i-enable-undercurl-curly-underlines to setup the terminfo
alias v='env TERM=wezterm nvim'
alias vim='env TERM=wezterm nvim'

alias vf='v $(fasd -lf | fzf)'
