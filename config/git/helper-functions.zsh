#!/usr/bin/env zsh

git-search-prs() {
  echo $(git pr list | \
   sed "s/^[ \t]*//" | \
    fzf --height=20% | \
    parse-number-from-hub-list)
}

git-update-with-branch() {
  if [ -z "$1" ]; then
    branch="master"
  else
    branch="$1"
  fi

  git fetch origin "$branch" && \
  git rebase origin/$branch
}

parse-number-from-hub-list() {
  cut -d" " -f1 | tr -dc "0-9"
}

git-show-issue-from-list() {
  local number
  number=$(echo "$1" | parse-number-from-hub-list)
  hub issue show "$number"
}

git-search-issues() {
  echo $(git issue | \
 sed "s/^[ \t]*//" | \
 fzf --height=40%  \
     --preview='hub issue show $(echo '{}' | cut -d" " -f1 | tr -dc "0-9")' |
 parse-number-from-hub-list )
}

git-browse-pr() {
  local number
  number="$1"
  if [ -z "$number" ]; then
    number=$(git-search-prs)
  fi

  if [ ! -z "$number" ]; then
    git browse -- "pull/$number"
  fi
}

git-browse-issue() {
  local number
  number="$1"
  if [ -z "$number" ]; then
    number=$(git-search-issues)
  fi

  if [ ! -z "$number" ]; then
    git browse -- "issues/$number"
  fi
}


git-checkout-pull() {
  local number
  number="$1"
  if [ -z "$number" ]; then
    number=$(git-search-prs)
  fi

  if [ ! -z "$number" ]; then
    hub pr checkout "$number"
  fi
}

git-commit-push() {
  local message
  if [ $# -eq 1 ]; then
    message="-m \"$1\""
  fi
  git commit $message && git push
}

git-remove-inactive-repos() {
  local repos=($(find "$1" -type d -mindepth 1 -maxdepth 1))
  local timestamp status
  for repo in "${repos[@]}"; do
    git-remove-repo-if-inactive "$repo"
  done
}

git-has-changes() {
  local repo="$1"
  local git_status=$(git \
    --git-dir="$repo/.git" \
    --work-tree="$repo" \
    status \
    --short)
  return $([ ! -z "$git_status" ])
}

git-last-commit-timestamp() {
  local repo="$1"
  git \
    --git-dir "$repo/.git" \
    show \
    --pretty=format:%ct \
    --quiet
}

git-remove-repo-if-inactive() {
  local one_month_ago=$(gdate +%s -d "1 month ago")
  if [ -d "$repo/.git" ] && [ $(git-last-commit-timestamp "$repo") -lt $one_month_ago ]; then
    if git-has-changes "$repo"; then
      read "?$repo has uncommitted changes; do you still want to remove it (y/n)? " choice
      case "$choice" in
        n|N ) return;;
        * ) echo "invalid";;
      esac
    fi
    remove-folder "$repo"
  fi
}

remove-folder() {
  local folder="$1"
  printf "Removing %s\n" "$repo"
  local parent=$(dirname $PWD)
  rm -rf "$repo"
  cd "$parent"
}
