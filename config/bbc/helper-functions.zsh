export COSMOS_LOGIN_CACHE=~/.cosmos-logins

curl-bbc() {
  local verb url ttl body

  verb="$1"
  url="$2"

  if [ ! "$verb" = "GET" ]; then
    body="$3"
  elif [ -z "$3" ]; then
    ttl="0";
  else
    ttl="$3"
  fi

  if [ -z "$BBC_DEV_CERT" ]; then
     echo "BBC_DEV_CERT must be set in environment"
     return 1;
  fi

  if [ -z "$BBC_DEV_CERT_KEY" ]; then
     echo "$BBC_DEV_CERT_KEY must be set in environment"
     return 1
  fi

  curl-with-cache --silent \
                  --request "$verb" \
                  --header "Content-Type: application/json" \
                  --cert "$BBC_DEV_CERT" \
                  --key "$BBC_DEV_CERT_KEY" \
                  --ttl "$ttl" \
                  --data "$body" \
                  "$url"
}

curl-with-cache() {
  local method url ttl \
        cacheFolder fileAge \
        argsForCurl=()

  cacheFolder="$HOME/.curlcache"

  while [[ $# -gt 0 ]]; do
    item="$1"
    if [[ $# -eq 1 ]]; then
      url="$item"
      argsForCurl+=("$1")
      shift
      continue
    fi
    case "$item" in
      --request|-X)
        method="$2"
        argsForCurl+=("$1")
        argsForCurl+=("$method")
        shift
        shift
        ;;
      --ttl)
        ttl="$2"
        shift
        shift
        ;;
      *)
        argsForCurl+=("$1")
        shift
        ;;
    esac
  done

  if [ -z "$method" ]; then
    method="GET"
  fi

  if [ -z "$ttl" ]; then
     ttl=0
  fi

  cacheFile=$(echo "$method-$url" | md5)
  cachePath="$cacheFolder/$cacheFile"
  if ! get-from-cache "$cachePath" "$ttl"; then
    if [ ! -d "$cacheFolder" ]; then
      mkdir "$cacheFolder"
    fi
    curl "${argsForCurl[@]}" | tee "$cachePath"
  fi
}

get-from-cache() {
  local cacheFile ttl fileAge

  cacheFile="$1"
  ttl="$2"

  if [ -f "$cacheFile" ]; then
     fileAge=$(($(date +%s) - $(stat -f%c "$cacheFile")))
     if [ "$fileAge" -lt "$ttl" ]; then
       cat "$cacheFile"
       return
     fi
  fi
  return 1
}

curl-cosmos-api() {
  local verb method ttl

  verb="$1"
  method="$2"
  ttl="$3"

  curl-bbc \
    "$verb" \
    "https://cosmos.api.bbci.co.uk/v1/$method" \
    "$ttl"
}

cosmos-get-instance() {
  local service environment \
        index envPath

  service="$1"
  environment="$2"
  index="$3"
  envPath="services/$service/$environment"

  if [ -z "$index" ]; then
    index=0
  fi

  curl-cosmos-api \
    GET "$envPath/main_stack/instances" 216000 | \
    jq --raw-output ".instances[$index]"
}

cosmos-get-login() {
  local id
  id="$1"
  curl-cosmos-api \
    GET "logins/$id"
}

ssh-cosmos() {
  local service environment \
        login loginStatus \
        cacheFolder status \
        expiresAt expiresAtSeconds \
        cachedLogin region ip

  service="$1"
  environment="$2"
  cacheFolder="$HOME/.cosmos-ssh-logins"
  cacheFile="$cacheFolder/$(echo "$service-$environment")"
  region="$3"

  if [ ! -d "$cacheFolder" ]; then
    mkdir "$cacheFolder"
  fi

  if [ -z "$region" ]; then
    region="eu-west-1"
  fi

  if [ -f "$cacheFile" ]; then
    cachedLogin=$(cat $cacheFile)
    loginStatus=$(echo "$cachedLogin" | jq --raw-output ".status")
    if [ "$loginStatus" = "current" ]; then
      printf "Cached login found :-)\n"
      expiresAt=$(echo "$cachedLogin" | jq --raw-output .expires_at)
      expiresAtSeconds=$(python -c "import dateutil.parser; print(dateutil.parser.parse('$expiresAt').strftime('%s'))")
      if [ "$expiresAtSeconds" -gt $(date +%s) ]; then
        login="$cachedLogin"
      else
        printf "Expired :-(\n"
      fi
    fi
  fi

  if [ -z "$login" ]; then
    printf "Cached login data was not found :-( creating a new one using the Cosmos API\n"
    login=$(cosmos-create-login "$service" "$environment" | tee "$cacheFile")
  fi

  ip=$(echo "$login" | jq --raw-output ".instance_private_ip")
  echo "Connecting to $ip in $region via ssh..."
  ssh "$ip,$region"
}

cosmos-create-login() {
  local service environment \
        instanceId loginStatus \
        login loginId

  service="$1"
  environment="$2"
  envPath="services/$service/$environment"
  loginStatus="pending_creation"
  instanceId=$(cosmos-get-instance "$service" "$environment" | jq --raw-output ".id")
  loginId=$(curl-cosmos-api POST "$envPath/logins" "{ \"instance_id\": \"$instanceId\" }" | jq --raw-output .login.ref | xargs basename)
  while [ "$loginStatus" = "pending_creation" ] || [ "$loginStatus" = "creating" ]; do
    login=$(cosmos-get-login "$loginId")
    loginStatus=$(echo $login | jq --raw-output ".status")
  done
  echo "$login"
}

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
    local ip=$(cosmos login "$component" "$environment" | tail -n 1 | rev | cut -d " " -f 1 | rev)
    if [[ $? -eq 0 ]]; then
      echo $ip | tee $cachefile
    fi
  fi
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

create-bbc-repo() {
  local repo dir
  repo="$1"
  dir="$BBC_WORKSPACE/$repo"
  mkdir "$dir"
  cd "$dir"
  git init
  hub create "bbc/$repo";
}

bbc-repo() {
  local repo
  repo="$1"
  if [ -z "$repo" ]; then
    repo=$(tree "$BBC_WORKSPACE" -di -L 1 | sed '$d' | sed '$d' | tail +2 | fzf --print-query | tail -n 1)
  fi
  local dir="$BBC_WORKSPACE/$repo"
  if [ ! -d "$dir" ]; then
    if ! git clone "https://github.com/bbc/$repo.git" "$dir"; then
      read "?Do you want to create bbc/$repo? " choice
      case "$choice" in
        y|Y|Yes|yes ) create-bbc-repo "$repo" return;;
      esac
    fi
  fi
  cd "$dir"
}

clean-bbc-repo() {
  git-remove-inactive-repos "$BBC_WORKSPACE"
}
