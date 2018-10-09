alias jticket='echo $(jira list | fzf) | cut -d ":" -f 1'
alias jsprint='echo $(jira sprint | fzf) | cut -d ":" -f 1'
alias jtrans='jira_transition_ticket_fzf'
alias jb='jira_browse'
alias jpb='jira_print_board'
alias jt='jira_take'

