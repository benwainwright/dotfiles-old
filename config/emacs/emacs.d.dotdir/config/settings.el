(provide 'settings)

;; Disable menu bar
(menu-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbars
(scroll-bar-mode -1)

;; Highlight current line
;; (global-hl-line-mode +1) ;; This appears kind of broken in Emacs 27

;; Get rid of initial scratch message
(setq initial-scratch-message "")

;; Get rid of startup minibuffer message
(setq inhibit-startup-message t)

;; Persist undo history between sessions
(defvar undo-tree-auto-save-history)
(setq undo-tree-auto-save-history t)

;; Turn on relative line numbers
(defvar display-line-numbers-type)
(setq display-line-numbers-type 'relative)

;; Automatically insert matching delimeter pairs
(electric-pair-mode t)

;; Don't warn when opening large files
(setq large-file-warning-threshold 100000000)

(setq visible-bell t)
