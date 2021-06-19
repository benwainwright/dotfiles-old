alias g='git'
alias grm='git fetch --all && git rebase origin/master'
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
alias gha='git rev-parse --short HEAD'
alias ga='git add'
alias gap='git add -p'
alias gaa='git add -A'
alias gco='git checkout'
alias gcom='git checkout $(git_main_branch)'
alias gcob='git checkout -b'
alias gcop='git-checkout-pull'
alias gcm='git commit'
alias gcmp='git-commit-push'
alias gnuke='git reset . && git checkout . && git clean -fd'
alias gr='git reset'
alias gcps='commit-push'
alias branch='git branch -a | awk '\''{n = split($1, a, "/"); print a[n]}'\'' | sort -u | fzf'
alias vbranch='g branch -av | awk '\''{n = split($1, a, "/")} !seen[a[n]]++ { print; }'\'' | fzf | awk '\''{n = split($1, a, "/"); print a[n]}'\'
alias gfm='git pull'
alias gpr='git pull-request'
alias pr='[ ! -z $(git rev-parse --show-cdup) ] && cd $(git rev-parse --show-cdup || pwd)'
alias gb='git browse'
alias gbp='git-browse-pr'
alias gbi='git-browse-issue'
alias gup='git-update-with-branch'
alias glo='git log --oneline'
alias gpsup='git push --set-upstream origin $(git symbolic-ref --short HEAD)'
