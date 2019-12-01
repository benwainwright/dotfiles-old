(provide 'settings)

;; Disable menu bar
(menu-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable scrollbars
(scroll-bar-mode -1)

;; Highlight current line
(global-hl-line-mode +1)

;; Fix color problem on mac
(setq ns-use-srgb-colorspace nil)

;; Get rid of initial scratch message
(setq initial-scratch-message "")

;; Get rid of startup minibuffer message
(setq inhibit-startup-message t)

;; Persist undo history between sessions
(setq undo-tree-auto-save-history t)

;; Turn on relative line numbers
(setq display-line-numbers-type 'relative)

;; Automatically insert matching delimeter pairs
(electric-pair-mode t)
