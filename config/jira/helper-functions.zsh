jira_transition_ticket_fzf() {
  local ticket=$(jira sprint | fzf | cut -d ":" -f 1)
  local state=$(jira transitions "$ticket" | fzf | cut -d ":" -f 2 | xargs)
  jira transition --noedit "$state" "$ticket"
}

print_jira_board() {
  local first_ticket=$(jira sprint | head -n 1 | cut -d ":" -f 1)
  local transitions=($(jira transitions $first_ticket | cut -d ":" -f 2 | awk '{$1=$1};1' | sed 's/ /_/g'))

  for transition in "${transitions[@]}"; do
    transition=$(echo $transition | sed 's/_/ /g')
    echo "\n -----------------------------"
    echo "       $transition"
    echo " -----------------------------"
    jira list --query="project = \"ORBITEN\" AND status = \"$transition\" AND Sprint in openSprints() AND sprint not in futureSprints()"
  done
 }

