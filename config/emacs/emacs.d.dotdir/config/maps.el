(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(global-set-key (kbd "C-n") 'next-buffer)
(global-set-key (kbd "C-p") 'previous-buffer)
(global-set-key (kbd "s-p") 'helm-projectile-switch-project)
(global-set-key (kbd "s-g") 'helm-projectile-rg)
(global-set-key (kbd "s-c") 'projectile-compile-project)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-file)
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-a") 'org-agenda)
(global-set-key (kbd "C-t") 'treemacs)

(defvar leader-map (make-sparse-keymap)
  "Keymap for \"leader\" shortcuts.")

(unbind-key "<SPC>" evil-motion-state-map)
(define-key evil-normal-state-map (kbd "SPC") leader-map)

;; Shortcuts
(define-key leader-map "c" 'magit-branch-checkout)
(define-key leader-map "s" 'magit-status)
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

;; Dash
(define-key leader-map "dap" 'dash-at-point)
