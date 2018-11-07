
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

  curl \
    --fail \
    --silent \
    --show-error \
    --globoff \
    --request "$method" \
    --cert "$DEV_CERT_LOCATION" \
    --key "$DEV_KEY_LOCATION" \
    --user "$JENKINS_USER:$JENKINS_TOKEN" \
    "$JENKINS_URL/$api_request"
}

jenkins_build_path() {
  local build_path=$1
  local build_number=$2
  echo "job/${${build_path}//\///job/}/$build_number"
}

colored_output() {
  local output
  output=("$@")
  for line in "${output[@]}"; do
    if [[ $line =~ "FAILED" ]] || [[ $line =~ "FAILURE" ]]; then
      printf "\e[31m%s\e[39m\n" "$line"
    elif [[ $line =~ "SUCCESS" ]]; then
      printf "\e[32m%s\e[39m\n" "$line"
    elif [[ $line =~ "PAUSED" ]]; then
      printf "\e[34m%s\e[39m\n" "$line"
    elif [[ $line =~ "UNSTABLE" ]]; then
      printf "\e[33m%s\e[39m\n" "$line"
    elif [[ $line =~ "IN_PROGRESS" ]]; then
      printf "\e[35m%s\e[39m\n" "$line"
    else
      printf "%s\n" "$line"
    fi
  done
}

jenkins_build() {
  local job_name="$1"
  jenkins_request POST job/$job_name/build
}

jenkins_jobs() {
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

jenkins_job_build_history() {
  local request output filter
  request="$(jenkins_build_path $1 $2)/api/json?tree=builds[displayName,timestamp,result]"
  IFS=$'\n'
  filter='.builds[] |
    [
      .displayName,
      (.timestamp |. / 1000 | strftime("%l:%M%p %Y-%m-%d")),
      .result
    ] |
    @csv'

  output=($(jenkins_request GET "$request" \
    | jq -r $filter \
    | tr -d '"' \
    | tr ',' ' ' \
    | column -t -s $'\t'))
  
  colored_output "${output[@]}"
}

jenkins_pipeline_status() {
  local build_number request
  build_number=$2
  request="$(jenkins_build_path $1 $2)/wfapi/"
  if [[ $? -eq 0 ]]; then

    IFS=$'\n'
    local output=($(jenkins_request GET "$request" \
      | jq -r '.stages[] | [.name, .status] | @tsv' \
      | column -t -s $'\t' \
      | sed 's/^/ /'))

    colored_output "${output[@]}"
  fi
}

jenkins_build_log() {
  local request="$(jenkins_build_path $1 $2)/consoleText"
  jenkins_request GET "$request"
}

jenkins_build_metadata() {
  local output request
  request="$(jenkins_build_path $1 $2)/api/json/"
  output="$(jenkins_request GET "$request")"
  if [[ $? -eq 0 ]]; then
    filter='
    [ "Display name", .fullDisplayName ],
    [ "Id", .id],
    [ "Started by", (.actions[] | select(."_class" == "hudson.model.CauseAction") | .causes[].userName) ],
    [ "In progress", .building ],
    [ "Started", (.timestamp |. / 1000 | strftime("%I:%M%p %Y-%m-%d"))],
    [ "Result", .result ] | @tsv'

    IFS=$'\n'
    newOutput=($(echo "$output" \
      | tr -cd "[:print:]" \
      | jq -r $filter \
      | column -t -s $'\t' \
      | sed 's/^/ /'))

    colored_output "${newOutput[@]}"
  fi
}

jenkins_build_info() {
  jenkins_build_metadata "$1" "$2"
  echo "\n Pipeline Status"
  jenkins_pipeline_status "$1" "$2"
}
