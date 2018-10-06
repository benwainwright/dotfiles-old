# The script below was installed by running `cosmos completion >> (script)`
# I originally had it runnig each time the shell started but that was too slow

# Copyright (c) npm, Inc. and Contributors
# All rights reserved.

###-begin-cosmos-completion-###
### credits to npm, this file is coming directly from isaacs/npm repo
#
# Just testing for now. (trying to learn this cool stuff)
#
# npm command completion script
#
# Installation: cosmos completion >> ~/.bashrc  (or ~/.zshrc)
#

COMP_WORDBREAKS=${COMP_WORDBREAKS/=/}
COMP_WORDBREAKS=${COMP_WORDBREAKS/@/}
export COMP_WORDBREAKS

if type complete &>/dev/null; then
  _cosmos_completion () {
    local si="$IFS"
    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$COMP_CWORD" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           cosmos completion -- "${COMP_WORDS[@]}" \
                           2>/dev/null)) || return $?
    IFS="$si"
  }
  complete -F _cosmos_completion cosmos
elif type compctl &>/dev/null; then
  _cosmos_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       cosmos completion -- "${words[@]}" \
                       2>/dev/null)) || return $?
    IFS="$si"
  }
  # if the completer function returns on matches, default
  # to filesystem matching
  compctl -K _cosmos_completion + -f + cosmos
fi
###-end-cosmos-completion-###

