;;; org-settings.el --- All my org-mode settings in one place

(defvar org-agenda-files)
(setq org-agenda-files '("~/Dropbox (BBC)/org/gtd/inbox.org"
			 "~/Dropbox (BBC)/org/gtd/gtd.org"
			 "~/Dropbox (BBC)/org/gtd/tickler.org"))

(defvar org-capture-templates)
(setq org-capture-templates '(("t" "Todo [inbox]" entry
			       (file+headline "~/Dropbox (BBC)/org/gtd/inbox.org" "Tasks")
			       "* TODO %i%?")
			      ("T" "Tickler" entry
			       (file+headline "~/Dropbox (BBC)/org/gtd/tickler.org" "Tickler")
			       "* %i% \n%U")))

(defvar org-refile-targets)
(setq org-refile-targets '(("~/Dropbox (BBC)/org/gtd/gtd.org" :maxlevel . 3)
			   ("~/Dropbox (BBC)/org/gtd/someday.org" :level . 1)
			   ("~/Dropbox (BBC)/org/gtd/tickler.org" :maxlevel . 2)))

(defvar org-todo-keywords)
(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "DOING" "|" "DONE" "CANCELLED")))

;;open agenda in current window
(defvar org-agenda-window-setup)
(setq org-agenda-window-setup (quote current-window))

;;warn me of any deadlines in next 7 days
(defvar org-deadline-warning-days)
(setq org-deadline-warning-days 7)

;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(defvar org-agenda-todo-ignore-deadlines)
(setq org-agenda-todo-ignore-deadlines (quote all))

(defvar org-agenda-todo-ignore-scheduled)
(setq org-agenda-todo-ignore-scheduled (quote all))

;;sort tasks in order of when they are due and then by priority
(defvar org-agenda-sorting-strategy)
(setq org-agenda-sorting-strategy
      (quote
       ((agenda deadline-up priority-down)
	(todo priority-down category-keep)
	(tags priority-down category-keep)
	(search category-keep))))

(defvar org-priority-faces)
(setq org-priority-faces
      '((65 :foreground "red" :background "yellow")
	(66 :foreground "black" :background "yellow")
	(67 . "blue")))
      
(defvar org-agenda-start-day)
(setq org-agenda-start-day "-1d")

(defvar org-agenda-span)
(setq org-agenda-span 7)

(defvar org-agenda-start-on-weekday)
(setq org-agenda-start-on-weekday nil)
