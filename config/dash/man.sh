#!/usr/bin/bash
encodeuri() {
  local string="${@}"
  local strlen=${#string}
  local encoded=""

  for (( pos = 0; pos < strlen; pos ++ )); do
    c=${string:$pos:1}
    case "$c" in
      [-_.~a-zA-Z0-9]) o="${c}" ;;
      *) printf -v o '%%%02x' "'$c"
    esac
    encoded+="${o}"
  done
  echo "${encoded}"
}

function man {
  if [[ -d /Applications/Dash.app && \
    -d "$HOME/Library/Application Support/Dash/DocSets/Man_Pages" ]]; then
    /usr/bin/open dash://manpages:"$(encodeuri "${@}")"
  else
    /usr/bin/man "${@}"
  fi
}
