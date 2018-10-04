
jenkins_request() {
  local method="$1"
  local api_request="$2"

  if [[ -z "$JENKINS_URL" ]]; then
    echo "JENKINS_URL not set..."
    return
  fi

  if [[ -z "$JENKINS_USER" ]]; then
    echo "JENKINS_USER not set..."
    return
  fi
  
  if [[ -z "$JENKINS_TOKEN" ]]; then
    echo "JENKINS_TOKEN not set..."
    return
  fi

  if [[ -z "$DEV_CERT_LOCATION" ]]; then
    echo "DEV_CERT_LOCATION not set..."
    return
  fi

  if [[ -z "$DEV_KEY_LOCATION" ]]; then
    echo "DEV_KEY_LOCATION not set..."
    return
  fi

  curl -g -s -X "$method" "$JENKINS_URL/$api_request" \
       --cert "$DEV_P12_LOCATION:$DEV_P12_PASSWORD" \
       --key "$DEV_KEY_LOCATION" \
       --user "$JENKINS_USER:$JENKINS_TOKEN"
}

build_jenkins_job() {
  local job_name="$1"
  jenkins_request POST job/$job_name/build
}

get_jenkins_jobs() {
  local request
  if [[ -z "$1" ]]; then
    request="api/json"
  else
    request="job/$1/api/json"
  fi
  jenkins_request GET "$request" | jq -r '.jobs[].name'
}

get_jenkins_job_build_history() {
  request="job/${${1}//\///job/}/api/json?tree=builds[displayName,timestamp,result]"
  jenkins_request GET "$request" | jq -r '.builds[] | [.displayName, (.timestamp |.  / 1000 | strftime("%l:%M%p %Y-%m-%d")), .result ] | @csv' | tr -d '"' | tr ',' ' '
}
