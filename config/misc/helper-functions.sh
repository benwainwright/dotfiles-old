
nice-tree() {
  local gitignore_pattern=$(cat $HOME/.gitignore_global | xargs | tr ' ' '|' | tr -d '\n')
  tree -aC -I "$gitignore_pattern"
}
