ssh-cosmos() {
  local component="$1"
  local environment="$2"
  local ip=$(cosmos login "$component" "$environment" | tail -n 1 | rev | cut -d " " -f 1 | rev)
  ssh $ip,eu-west-1
}
