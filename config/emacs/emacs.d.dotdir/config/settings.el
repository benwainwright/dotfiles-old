(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode +1)
(global-font-lock-mode 1)

(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)

(setq backup-directory-alist `(("." . "~/.emacssaves")))
(setq custom-file "~/.emacs.d/custom.el")
(setq display-line-numbers-type 'relative)
(load custom-file)

