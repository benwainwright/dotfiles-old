install_gobrew() {
  if command -v gobrew > /dev/null; then
    curl -sLk https://raw.githubusercontent.com/kevincobain2000/gobrew/master/git.io.sh | sh
  fi
}
