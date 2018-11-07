
jenkins_request() {
  local method="$1"
  local api_request="$2"

  if [[ -z "$JENKINS_URL" ]]; then
    echo "JENKINS_URL not set..."
    return 1
  fi

  if [[ -z "$JENKINS_USER" ]]; then
    echo "JENKINS_USER not set..."
    return 1
  fi
  
  if [[ -z "$JENKINS_TOKEN" ]]; then
    echo "JENKINS_TOKEN not set..."
    return 1
  fi

  if [[ -z "$DEV_CERT_LOCATION" ]]; then
    echo "DEV_CERT_LOCATION not set..."
    return 1
  fi

  if [[ -z "$DEV_KEY_LOCATION" ]]; then
    echo "DEV_KEY_LOCATION not set..."
    return 1
  fi

  curl --fail \
       --silent \
       --globoff \
       --request "$method" \
       --cert "$DEV_CERT_LOCATION" \
       --key "$DEV_KEY_LOCATION" \
       --user "$JENKINS_USER:$JENKINS_TOKEN" \
       "$JENKINS_URL/$api_request" \
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
  if ! jenkins_request GET "$request" | jq -r '.jobs[].name'; then
    echo "Failed to get Jenkins jobs..."
  fi
}

get_jenkins_job_build_history() {
  local request="job/${${1}//\///job/}/api/json?tree=builds[displayName,timestamp,result]"
  jenkins_request GET "$request" | jq -r '.builds[] | [.displayName, (.timestamp |.  / 1000 | strftime("%l:%M%p %Y-%m-%d")), .result ] | @csv' | tr -d '"' | tr ',' ' '
}

get_pipeline_status() {
  local build_number=$2
  local request="job/${${1}//\///job/}/$build_number/wfapi/"
  IFS=$'\n'
  local output=($(jenkins_request GET "$request" | jq -r '.stages[] | [.status, .name] | @tsv'))
  for line in "${output[@]}"; do
    if [[ $line =~ "FAILED" ]]; then
      printf "\e[31m%s\e[39m\n" "$line"
    elif [[ $line =~ "SUCCESS" ]]; then
      printf "\e[32m%s\e[39m\n" "$line"
    elif [[ $line =~ "PAUSED" ]]; then
      printf "\e[34m%s\e[39m\n" "$line"
    elif [[ $line =~ "UNSTABLE" ]]; then
      printf "\e[33m%s\e[39m\n" "$line"
    else
      printf "%s\n" "$line"
    fi
  done
}
