jira_transition_ticket_fzf() {
  local ticket=$(jira sprint | fzf | cut -d ":" -f 1)
  local state=$(jira transitions "$ticket" | fzf | cut -d ":" -f 2 | xargs)
  jira transition --noedit "$state" "$ticket"
}

jira_browse() {
  jira browse ORBITEN-$1
}

jira_take() {
  jira take ORBITEN-$1
}


# Yes this function is kind of horrible 
# but it does the job, for now
jira_print_board() {
  IFS=$'\n'
  local tickets=($(jira sprint))
  local open=()
  local ready_for_dev=()
  local dev_wip=()
  local in_review=()
  local ready_to_deploy=()
  local on_int=()
  local ready_for_test=()
  local test_wip=()
  local ready_for_release=()
  local closed_wont_do=()
local ticket_status ticket_priority

  local statuses=(\
    "Open" \
    "Ready for Dev")


  for raw_ticket in "${tickets[@]}"; do
    ticket_status=$(echo $raw_ticket | cut -d ";" -f 2)
    ticket_priority=$(echo $raw_ticket | cut -d ";" -f 3)
    ticket=$(echo $raw_ticket | cut -d ";" -f 1)
    case "$ticket_status" in
      "Open")
        open+=("$ticket;$ticket_priority")
        ;;
      "Ready for Dev")
        ready_for_dev+=("$ticket;$ticket_priority")
        ;;
      "Dev - WIP")
        dev_wip+=("$ticket;$ticket_priority")
        ;;
      "In Review")
        in_review+=("$ticket;$ticket_priority")
        ;;
      "Ready to deploy")
        ready_to_deploy+=("$ticket;$ticket_priority")
        ;;
      "On Int")
        on_int+=("$ticket;$ticket_priority")
        ;;
      "Ready for test")
        ready_for_test+=("$ticket;$ticket_priority")
        ;;
      "Test - WIP")
        test_wip+=("$ticket;$ticket_priority")
        ;;
      "Ready for Release")
        ready_for_release+=("$ticket;$ticket_priority")
        ;;
      "Closed - Won't do")
        closed_wont_do+=("$ticket;$ticket_priority")
        ;;
    esac
  done
 
  jira_print_status_tickets "Open" "${open[@]}" 
  jira_print_status_tickets "Ready for Dev" "${ready_for_dev[@]}"    
  jira_print_status_tickets "Dev - WIP" "${dev_wip[@]}" 
  jira_print_status_tickets "In Review" "${in_review[@]}" 
  jira_print_status_tickets "Ready to deploy" "${ready_to_deploy[@]}" 
  jira_print_status_tickets "On Int" "${on_int[@]}" 
  jira_print_status_tickets "Ready for Test" "${ready_for_test[@]}" 
  jira_print_status_tickets "Test - WIP"  "${test_wip[@]}" 
  jira_print_status_tickets "Ready for release"  "${ready_for_release[@]}" 
  # jira_print_status_tickets "Closed won't do" "${closed_wont_do[@]}"
 }

jira_print_status_tickets() {
  local name="$1"
  shift
  local status_array=("$@")
  if [ "${#status_array}" -ne 0 ]; then
    tput setaf 2
    local cols=$(tput cols)
    local title="====== $name "
    local right_bar_width=$(($cols - ${#title})) 
    printf "\n%s%s" "$title" $(printf %${right_bar_width}s | tr " " "=")
    tput sgr0
    for item in "${status_array[@]}"; do
      local ticket=$(echo $item | cut -d ";" -f 1)
      local priority=$(echo $item | cut -d ";" -f 2)
      if [ "$priority" = "Blocker" ]; then
        tput setaf 1
      fi
      printf "%s\n" "$ticket"
    tput sgr0
    done
  else
    tput setaf 4
    printf "\n(%s)\n" "$name is empty"
    tput sgr0
  fi
}

