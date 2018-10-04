aws_cli_profile_set() {
  export AWS_DEFAULT_PROFILE=$1
}

aws() {
  if [[ -z "$AWS_DEFAULT_PROFILE" ]]; then
    echo "\$AWS_DEFAULT_PROFILE is not set. Run aws_cli_profile_set (profile-name)"
  else
    aws "$@"
  fi
}
