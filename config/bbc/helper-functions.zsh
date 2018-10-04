ssh-cosmos() {
  local component="$1"
  local environment="$2"
  echo "Getting cosmos login for '$component' on '$environment'..."
  local ip=$(cosmos login "$component" "$environment" | tail -n 1 | rev | cut -d " " -f 1 | rev)
  ssh $ip,eu-west-1
}
