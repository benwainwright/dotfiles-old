symlink() {
  command="ln -sfn"
  echo "Symlinking $1 -> $2"
  eval "$command $1 $2"
}
