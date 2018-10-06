ssh-cosmos() {
  local component="$1"
  local environment="$2"
  echo "Getting cosmos login for '$component' on '$environment'..."
  local ip=$(cosmos login "$component" "$environment" | tail -n 1 | rev | cut -d " " -f 1 | rev)
  ssh $ip,eu-west-1
}

bbc-repo() {
  local dir="$BBC_WORKSPACE/$1"
  if [ ! -d "$dir" ]; then
    if hub clone "bbc/$1" $dir; then
      cd "$dir"
    fi
  fi
  cd "$dir"
}

clean-bbc-repo() {
  git-remove-inactive-repos "$BBC_WORKSPACE"
}
