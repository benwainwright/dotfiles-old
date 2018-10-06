#!/usr/bin/env zsh

commit-push() {
  local message
  if [ $# -eq 1 ]; then
    message="-m \"$1\""
  fi
  git commit $message && git push
}

remove-inactive-repos() {
  local repos=($(find "$1" -type d -mindepth 1 -maxdepth 1))
  local timestamp status
  for repo in "${repos[@]}"; do
    remove-repo-if-inactive "$repo"
  done
}

remove-repo-if-inactive() {
  local one_month_ago=$(gdate +%s -d "1 month ago")
  if [ -d "$repo/.git" ]; then
    timestamp=$(git --git-dir "$repo/.git" show --pretty=format:%ct --quiet)
    if [ $timestamp -lt $one_month_ago ]; then 
      git_status=$(git --git-dir="$repo/.git" --work-tree="$repo" status --short)
      if [ ! -z "$git_status" ]; then
        read "?$repo has uncommitted changes; do you still want to remove it (y/n)? " choice
        case "$choice" in 
          n|N ) return;;
          * ) echo "invalid";;
        esac
      fi
      echo "Removing $repo"
      rm -rf "$repo"
    fi
  fi
}
