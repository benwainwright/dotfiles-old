alias g='git'
alias gs='git stash'
alias gp='git push'
alias gdt='git difftool'
alias gia='git add'
alias gnuke='git checkout . && git clean -f'
alias gcps='commit-push'
alias branch='git branch -a | awk '\''{n = split($1, a, "/"); print a[n]}'\'' | sort -u | fzf'
alias vbranch='g branch -av | awk '\''{n = split($1, a, "/")} !seen[a[n]]++ { print; }'\'' | fzf | awk '\''{n = split($1, a, "/"); print a[n]}'\'
alias git=hub

