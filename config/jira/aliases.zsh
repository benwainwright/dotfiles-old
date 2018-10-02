alias jticket='echo $(jira list | fzf) | cut -d ":" -f 1'
alias jsprint='echo $(jira sprint | fzf) | cut -d ":" -f 1'

