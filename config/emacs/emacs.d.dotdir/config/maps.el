(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(define-minor-mode my-global-keymaps-mode
  "Personal global keybindings"
  :keymap (make-sparse-keymap)
  :init-value t)

(define-key my-global-keymaps-mode-map (kbd "C-n") 'next-buffer)
(define-key my-global-keymaps-mode-map (kbd "C-p") 'previous-buffer)
(define-key my-global-keymaps-mode-map (kbd "s-p") 'helm-projectile-switch-project)
(define-key my-global-keymaps-mode-map (kbd "s-g") 'helm-projectile-rg)
(define-key my-global-keymaps-mode-map (kbd "s-c") 'projectile-compile-project)
(define-key my-global-keymaps-mode-map (kbd "M-x") 'helm-M-x)
(define-key my-global-keymaps-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key my-global-keymaps-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key my-global-keymaps-mode-map (kbd "M-p") 'flycheck-previous-error)
(define-key my-global-keymaps-mode-map (kbd "C-j") 'windmove-down)
(define-key my-global-keymaps-mode-map (kbd "C-k") 'windmove-up)
(define-key my-global-keymaps-mode-map (kbd "C-h") 'windmove-left)
(define-key my-global-keymaps-mode-map (kbd "C-l") 'windmove-right)
(define-key my-global-keymaps-mode-map (kbd "C-c c") 'org-capture)
(define-key my-global-keymaps-mode-map (kbd "C-a") 'org-agenda)
(define-key my-global-keymaps-mode-map (kbd "C-t") 'treemacs)

(my-global-keymaps-mode)

(defvar leader-map (make-sparse-keymap)
  "Keymap for \"leader\" shortcuts.")

; (unbind-key "<SPC>" evil-motion-state-map)
(define-key evil-normal-state-map (kbd "SPC") leader-map)
(define-key evil-visual-state-map (kbd "SPC") leader-map)

;; Shortcuts
(define-key leader-map "c" 'magit-branch-checkout)
(define-key leader-map "p" 'magit-pull)
(define-key leader-map "r" 'reload-init-file)

;; Git and Github related
(define-key leader-map "gcp" 'forge-checkout-pullreq)
(define-key leader-map "gbr" 'browse-at-remote)
(define-key leader-map "gbc" 'browse-at-remote)
(define-key leader-map "gib" 'forge-browse-issue)
(define-key leader-map "gic" 'forge-create-issue)
(define-key leader-map "gpc" 'forge-create-pullreq)
(define-key leader-map "gpb" 'forge-browse-pullreq)
(define-key leader-map "grl" 'magit-reflog-current)
(define-key leader-map "gri" 'magit-rebase-interactive)
(define-key leader-map "gcl" 'magit-clone)
(define-key leader-map "gfa" 'magit-fetch-all)
(define-key leader-map "gcm" 'magit-commit)
(define-key leader-map "gs" 'magit-status)
(define-key leader-map "gl" 'magit-log-current)
(define-key leader-map "hs" 'git-gutter:stage-hunk)
(define-key leader-map "hp" 'git-gutter:previous-hunk)
(define-key leader-map "hn" 'git-gutter:next-hunk)
(define-key leader-map "hd" 'git-gutter:popup-hunk)

;; Help
(define-key leader-map "dk" 'describe-key)
(define-key leader-map "dv" 'describe-variable)
(define-key leader-map "df" 'describe-function)
(define-key leader-map "dp" 'desribe-package)
(define-key leader-map "db" 'describe-bindings)
(define-key leader-map "dm" 'describe-mode)
(define-key leader-map "ds" 'describe-syntax)

;; Evaluate Elisp
(define-key leader-map "ed" 'eval-defun)
(define-key leader-map "er" 'eval-region)
(define-key leader-map "eb" 'eval-buffer)
(define-key leader-map "els" 'eval-last-sexp)
(define-key leader-map "ee" 'eval-expression)

;; Flycheck
(define-key leader-map "fl" 'flycheck-list-errors)
(define-key leader-map "fn" 'flycheck-next-error)
(define-key leader-map "fn" 'flycheck-prev-error)

;; Flyspell
(define-key leader-map "sc" 'flyspell-correct-word-before-point)
(define-key leader-map "sn" 'flyspell-goto-next-error)

;; Dash
(define-key leader-map "dap" 'dash-at-point)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
