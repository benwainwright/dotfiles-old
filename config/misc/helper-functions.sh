
nice-tree() {
  local gitignore_pattern=$(cat $HOME/.gitignore_global | xargs | tr ' ' '|' | tr -d '\n')
  tree -aC -I "$gitignore_pattern"
}

listening-on-port() {
  lsof -i :$1 | sed '1p;/LISTEN/!d'
}

cj() {
  curl --silent "$@" | jq .
}
