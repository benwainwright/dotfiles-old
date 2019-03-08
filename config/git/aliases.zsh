alias g='git'
alias gr='git rebase'
alias gri='git rebase -i'
alias gf='git fetch'
alias gfa='git fetch --all'
alias gs='git stash'
alias gst='git status'
alias gsa='git stash apply'
alias gp='git push'
alias gdt='git difftool'
alias gdts='git difftool --staged'
alias gdtc='git difftool --cached'
alias ga='git add'
alias gap='git add -p'
alias gaa='git add -A'
alias gco='git checkout'
alias gcom='git checkout master'
alias gcob='git checkout -b'
alias gcm='git commit'
alias gcmp='git-commit-push'
alias gnuke='git reset . && git checkout . && git clean -fd'
alias gr='git reset'
alias gcps='commit-push'
alias branch='git branch -a | awk '\''{n = split($1, a, "/"); print a[n]}'\'' | sort -u | fzf'
alias vbranch='g branch -av | awk '\''{n = split($1, a, "/")} !seen[a[n]]++ { print; }'\'' | fzf | awk '\''{n = split($1, a, "/"); print a[n]}'\'
alias git='hub'
alias gfm='git pull'
alias gpr='git pull-request'
alias pr='[ ! -z $(git rev-parse --show-cdup) ] && cd $(git rev-parse --show-cdup || pwd)'
alias gbp='git-browse-pull'
alias gpb='git browse -- pull/$(git pr list | sed "s/^[ \t]*//" | fzf --height=10% | awk "{print $1}" | tr -dc "0-9")'
