#!/usr/bin/env bash

jenkins() {
  case "$1" in
    build)
      shift
      _jenkins_build "$@"
      ;;
    list)
      shift
      _jenkins_jobs "$@"
      ;;
  esac
}

_jenkins_build() {
  case "$1" in
    run)
      shift
      _jenkins_run_build "$@"
      ;;
    info)
      shift
      _jenkins_build_info "$@"
      ;;
    log)
      shift
      _jenkins_build_log "$@"
      ;;
  esac
}

_jenkins_request() {
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

_jenkins_build_path() {
  local build_path
  build_path="$1"
  echo "job/${${build_path}//\///job/}"
}


_jenkins_run_build() {
  local build_path
  build_path="$1"
  _jenkins_request POST "$(_jenkins_build_path $build_path)/build"
}

_colored_output() {
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

_jenkins_jobs() {
  local request
  if [[ -z "$1" ]]; then
    request="api/json"
  else
    request="job/$1/api/json"
  fi
  if ! _jenkins_request GET "$request" | jq -r '.jobs[].name'; then
    echo "Failed to get Jenkins jobs..."
  fi
}

_jenkins_build_history() {
  local request output filter build_path
  build_path="$1"
  request="$(_jenkins_build_path $build_path)/api/json?tree=builds[displayName,timestamp,result]"
  IFS=$'\n'
  filter='.builds[] |
    [
      .displayName,
      (.timestamp |. / 1000 | strftime("%l:%M%p %Y-%m-%d")),
      .result
      ] |
        @csv'

  output=($(_jenkins_request GET "$request" \
    | jq -r $filter \
    | tr -d '"' \
    | tr ',' ' ' \
    | column -t -s $'\t'))

  _colored_output "${output[@]}"
}

_jenkins_pipeline_status() {
  local build_path build_number request
  build_path="$1"
  build_number="$2"
  request="$(_jenkins_build_path $build_path)/$build_number/wfapi/"
  if [[ $? -eq 0 ]]; then

    IFS=$'\n'
    local output=($(_jenkins_request GET "$request" \
      | jq -r '.stages[] | [.name, .status] | @tsv' \
      | column -t -s $'\t' \
      | sed 's/^/ /'))

    _colored_output "${output[@]}"
  fi
}

_jenkins_build_log() {
  local request build_path build_number
  build_path="$1"
  build_number="$2"
  request="$(_jenkins_build_path $build_path)/$build_number/consoleText"
  _jenkins_request GET "$request"
}

_jenkins_build_metadata() {
  local output request buld_path build_number
  build_path="$1"
  build_number="$2"
  request="$(_jenkins_build_path $build_path)/$build_number/api/json/"
  output="$(_jenkins_request GET "$request")"
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

      _colored_output "${newOutput[@]}"
    fi
  }

_jenkins_build_info() {
  local build_path build_number
  build_path="$1"
  build_number="$2"
  _jenkins_build_metadata "$1" "$2"
  echo "\n Pipeline Status"
  _jenkins_pipeline_status "$1" "$2"
}
