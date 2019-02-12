export DOCKER_HOSTS=( \
  "localhost"
  "grafana.toolshed.test.tools.bbc.co.uk" \
  "grafana.toolshed.tools.bbc.co.uk" \
  "jenkins.cd.tools.bbc.co.uk" \
  "jenkins.cd.test.tools.bbc.co.uk" \
)

function switch_docker_host() {
  local host

  if [ -z "$1" ]; then
    host=$(echo $DOCKER_HOSTS | tr " " "\n" | fzf --height=40%)
  else
    host="$1"
  fi

  if [ "$host" = "localhost" ]; then
    unset DOCKER_HOST DOCKER_TLS_VERIFY
  else
    export DOCKER_HOST="tcp://$host:2376"
    export DOCKER_TLS_VERIFY=1
  fi
}
