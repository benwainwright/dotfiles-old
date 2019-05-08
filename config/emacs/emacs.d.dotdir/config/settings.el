(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode +1)
(global-font-lock-mode 1)
(setq ns-use-srgb-colorspace nil)

(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq undo-tree-auto-save-history t)

(setq auto-revert-check-vc-info t)
(setq backup-directory-alist `(("." . "~/.emacssaves")))
(setq custom-file "~/.emacs.d/custom.el")
(setq display-line-numbers-type 'relative)
(load custom-file)

(load-theme 'adwaita)

(setq display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window)))

(electric-pair-mode t)


;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
