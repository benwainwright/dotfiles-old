(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode +1)
(global-font-lock-mode 1)
(global-display-line-numbers-mode)

(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)

(setq backup-directory-alist `(("." . "~/.emacssaves")))
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(defvar org-agenda-files)
(setq org-agenda-files (list "~/org/work.org"))
(setq display-line-numbers-type 'relative)
