jira_transition_ticket_fzf() {
  local ticket=$(jira sprint | fzf | cut -d ":" -f 1)
  local state=$(jira transitions "$ticket" | fzf | cut -d ":" -f 2 | xargs)
  jira transition --noedit "$state" "$ticket"
}

