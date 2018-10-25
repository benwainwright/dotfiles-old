export COSMOS_LOGIN_CACHE=~/.cosmos-logins

# Since the Cosmos CLI is so damn slow, this funcion requests a login
# and caches the IP in ~/.cosmos-logins until the login is 1 hour old
# (I got 1 hour from https://confluence.dev.bbc.co.uk/display/MyBBC/Cosmos+Production+SSH+access)
ssh-cosmos-ip() {
  local component="$1"
  local environment="$2"
  local keyname="$component-$environment"
  local cachefile=$COSMOS_LOGIN_CACHE/$keyname
  if [[ ! -d $COSMOS_LOGIN_CACHE ]]; then
    mkdir $COSMOS_LOGIN_CACHE
  fi
  if [[ -f $cachefile ]] && [[ ! -z $(find $cachefile -mmin -60) ]]; then
    cat $cachefile
  else
    echo $(cosmos login "$component" "$environment" | tail -n 1 | rev | cut -d " " -f 1 | rev | tee $cachefile)
  fi
}

ssh-cosmos() {
  local component="$1"
  local environment="$2"
  echo "Getting cosmos login for '$component' on '$environment'..."
  local ip=$(ssh-cosmos-ip "$component" "$environment")
  ssh -o LogLevel=ERROR $ip,eu-west-1
}

make-orbit-redis-tunnel() {
  local environment="$1"
  echo "Getting cosmos login for 'navigation' on '$environment'..."
  local ip=$(ssh-cosmos-ip "navigation" "$environment")
  # Kill anything listening on 6379
  lsof -i :6379 | tail -n +2 | awk '{print $2}' | sort -u | xargs kill -9
  local redis_host=$(ssh -o LogLevel=ERROR $ip,eu-west-1 "sudo cat /etc/bake-scripts/config.json" | jq --raw-output '.configuration.redis_host')
  echo "Getting redis_host from $ip,eu-west-1"
  ssh -o LogLevel=ERROR -f -N -L 6379:$redis_host:6379 $ip,eu-west-1
  echo "SSH tunnel to $redis_host:6379 via $ip,eu-west-1 created"
}

orbit-redis() {
  local environment="$1"
  make-orbit-redis-tunnel "$environment" && redis-cli
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
