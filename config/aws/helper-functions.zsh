aws_cli_profile_set() {
  export AWS_DEFAULT_PROFILE=$1
}

aws-switch-profile-fzf() {
  local profiles=$(cat ~/.aws/config | grep profile | sed 's/\[profile \(.*\)\]/\1/g')
  export AWS_DEFAULT_PROFILE=$(echo $profiles | fzf)
}

ecr-login() {
  if [ -z $AWS_DEFAULT_PROFILE ] || [ "$1" = "--switch" ]; then
    aws-switch-profile-fzf
  fi
  $(aws ecr get-login --no-include-email)
}

ecr-login-with-profile() {
  export AWS_DEFAULT_PROFILE="$1"
  $(aws ecr get-login --no-include-email)
}
