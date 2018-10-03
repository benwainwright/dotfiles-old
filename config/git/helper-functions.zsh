#!/usr/bin/env zsh

commit-push() {
  local message
  if [ $# -eq 1 ]; then
    message="-m \"$1\""
  fi
  git commit $message && git push
}
