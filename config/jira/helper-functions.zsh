jira_transition_ticket_fzf() {
  local ticket=$(jira sprint | fzf | cut -d ":" -f 1)
  local state=$(jira transitions "$ticket" | fzf | cut -d ":" -f 2 | xargs)
  jira transition --noedit "$state" "$ticket"
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
  local done=()
  local closed_wont_do=()
  local ticket_status

  for raw_ticket in "${tickets[@]}"; do
    ticket_status=$(echo $raw_ticket | cut -d ";" -f 2)
    ticket=$(echo $raw_ticket | cut -d ";" -f 1)
    case "$ticket_status" in
      "Ready for Dev")
        ready_for_dev+=("$ticket")
        ;;
      "Dev - WIP")
        dev_wip+=("$ticket")
        ;;
      "In Review")
        in_review+=("$ticket")
        ;;
      "Ready to deploy")
        ready_to_deploy+=("$ticket")
        ;;
      "On Int")
        on_int+=("$ticket")
        ;;
      "Ready for test")
        ready_for_test+=("$ticket")
        ;;
      "Test - WIP")
        test_wip+=("$ticket")
        ;;
      "Ready for Release")
        ready_for_release+=("$ticket")
        ;;
      "Done")
        done+=("$ticket")
        ;;
      "Closed - Won't do")
        closed_wont_do+=("$ticket")
        ;;
    esac
  done

  if [ "${#ready_for_dev}" -ne 0 ]; then
    printf "\n%s\n" "====== Ready for Dev ======"
    printf "%s\n" "$ready_for_dev"
  fi
  if [ "${#dev_wip}" -ne 0 ]; then
    printf "\n%s\n" "====== Dev - WIP ======"
    printf "%s\n" "$dev_wip"
  fi
  if [ "${#in_review}" -ne 0 ]; then
    printf "\n%s\n" "====== In Review ======"
    printf "%s\n" "$in_review"
  fi
  if [ "${#ready_to_deploy}" -ne 0 ]; then
    printf "\n%s\n" "====== Ready to deploy ======"
    printf "%s\n" "$ready_to_deploy"
  fi
  if [ "${#on_int}" -ne 0 ]; then
    printf "\n%s\n" "====== On Int ======"
    printf "%s\n" "$on_int"
  fi
  if [ "${#ready_for_test}" -ne 0 ]; then
    printf "\n%s\n" "====== Ready for Test ======"
    printf "%s\n" "$ready_for_test"
  fi
  if [ "${#test_wip}" -ne 0 ]; then
    printf "\n%s\n" "====== Test - WIP ======"
    printf "%s\n" "$test_wip"
  fi
  if [ "${#ready_for_release}" -ne 0 ]; then
    printf "\n%s\n" "====== Ready for release ======"
    printf "%s\n" "$ready_for_release"
  fi
  if [ "${#done}" -ne 0 ]; then
    printf "\n%s\n" "====== Done ======"
    printf "%s\n" "$done"
  fi
  if [ "${#closed_wont_do}" -ne 0 ]; then
    printf "\n%s\n" "====== Closed won't do ======"
    printf "%s\n" "$closed_wont_do"
  fi
 }
