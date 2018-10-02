alias gdt='git difftool'
alias branch='git branch -a | awk '\''{n = split($1, a, "/"); print a[n]}'\'' | sort -u | fzf'
alias vbranch='g branch -av | awk '\''{n = split($1, a, "/")} !seen[a[n]]++ { print; }'\'' | fzf | awk '\''{n = split($1, a, "/"); print a[n]}'\'
alias git=hub

