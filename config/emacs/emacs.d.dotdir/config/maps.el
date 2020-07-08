(provide 'maps)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(define-minor-mode my-global-keymaps-mode
  "Personal global keybindings"
  :keymap (make-sparse-keymap)
  :init-value t)

(define-key my-global-keymaps-mode-map (kbd "C-n")     'next-buffer)
(define-key my-global-keymaps-mode-map (kbd "C-p")     'previous-buffer)
(define-key my-global-keymaps-mode-map (kbd "s-p")     'helm-projectile-switch-project)
(define-key my-global-keymaps-mode-map (kbd "C-f")     'helm-projectile-find-file)
(define-key my-global-keymaps-mode-map (kbd "M-x")     'helm-M-x)
(define-key my-global-keymaps-mode-map (kbd "C-x C-f") 'helm-find-files)
(define-key my-global-keymaps-mode-map (kbd "M-n")     'flycheck-next-error)
(define-key my-global-keymaps-mode-map (kbd "M-p")     'flycheck-previous-error)
(define-key my-global-keymaps-mode-map (kbd "C-j")     'windmove-down)
(define-key my-global-keymaps-mode-map (kbd "C-k")     'windmove-up)
(define-key my-global-keymaps-mode-map (kbd "C-h")     'windmove-left)
(define-key my-global-keymaps-mode-map (kbd "C-l")     'windmove-right)
(define-key my-global-keymaps-mode-map (kbd "C-t")     'treemacs)
(define-key my-global-keymaps-mode-map (kbd "<f2>")     'lsp-rename)
(define-key my-global-keymaps-mode-map (kbd "<f12>")   'lsp-find-definition)
(define-key my-global-keymaps-mode-map (kbd "<S-f12>")   'lsp-ui-peek-find-references)
(define-key my-global-keymaps-mode-map (kbd "<f3>")   'bar-to-clipboard)
(define-key my-global-keymaps-mode-map (kbd "<S-f3>")   'browse-at-remote)
(define-key my-global-keymaps-mode-map (kbd "M-h")      'drag-stuff-left)
(define-key my-global-keymaps-mode-map (kbd "M-l")      'drag-stuff-right)
(define-key my-global-keymaps-mode-map (kbd "M-k")      'drag-stuff-up)
(define-key my-global-keymaps-mode-map (kbd "M-j")      'drag-stuff-down)
(define-key my-global-keymaps-mode-map (kbd "<f4>")   'my-string-inflection-cycle-auto)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(define-key my-global-keymaps-mode-map (kbd "C-a h") (lambda ()(interactive)(split-window-horizontally)))
(define-key my-global-keymaps-mode-map (kbd "C-a l") (lambda ()(interactive)(split-window-horizontally) (other-window 1)))
(define-key my-global-keymaps-mode-map (kbd "C-a j") (lambda ()(interactive)(split-window-vertically) (other-window 1)))
(define-key my-global-keymaps-mode-map (kbd "C-a k") (lambda ()(interactive)(split-window-vertically) ))


(defvar treemacs-mode-map)
(define-key treemacs-mode-map (kbd "<C-return>") 'treemacs-create-file)
(define-key treemacs-mode-map (kbd "M-RET") 'treemacs-create-dir)
(define-key treemacs-mode-map (kbd "<C-backspace>") 'treemacs-delete)

(my-global-keymaps-mode)

(defvar evil-normal-state-map)
(defvar evil-motion-state-map)
(unbind-key "C-f" evil-normal-state-map)
(unbind-key "C-f" evil-motion-state-map)
(unbind-key "C-p" evil-normal-state-map)
(unbind-key "C-p" evil-motion-state-map)
(unbind-key "C-n" evil-normal-state-map)
(unbind-key "C-n" evil-motion-state-map)
(unbind-key "C-t" evil-normal-state-map)
(unbind-key "C-t" evil-motion-state-map)
(bind-key ";" 'helm-buffers-list evil-motion-state-map)
(bind-key ";" 'helm-buffers-list evil-normal-state-map)
(bind-key "C-u" 'evil-scroll-up evil-motion-state-map)
(bind-key "C-u" 'evil-scroll-up evil-normal-state-map)

(defvar leader-map (make-sparse-keymap)
  "Keymap for \"leader\" shortcuts.")

					; (unbind-key "<SPC>" evil-motion-state-map)
(define-key evil-normal-state-map (kbd "SPC") leader-map)
(define-key evil-visual-state-map (kbd "SPC") leader-map)

;; Shortcuts
(define-key leader-map "c" 'counsel-git-checkout)

;; Git and Github related
(define-key leader-map "gbr" 'browse-at-remote)
(define-key leader-map "gbc" 'bar-to-clipboard)
(define-key leader-map "gib" 'forge-browse-issue)
(define-key leader-map "gic" 'forge-create-issue)
(define-key leader-map "gprn" 'forge-create-pullreq)
(define-key leader-map "gprc" 'forge-checkout-pullreq)
(define-key leader-map "gprb" 'forge-browse-pullreq)

(define-key leader-map "mrl" 'magit-reflog-current)
(define-key leader-map "mri" 'magit-rebase-interactive)
(define-key leader-map "mcl" 'magit-clone)
(define-key leader-map "mfa" 'magit-fetch-all)
(define-key leader-map "mcm" 'magit-commit)
(define-key leader-map "ms" 'magit-status)
(define-key leader-map "ml" 'magit-log-current)
(define-key leader-map "mcb" 'magit-branch-and-checkout)

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
(define-key leader-map "fsc" 'flyspell-correct-word-before-point)
(define-key leader-map "fsn" 'flyspell-goto-next-error)

;; Search
(define-key leader-map "sg" 'helm-projectile-ag)
(define-key leader-map "ss" 'helm-lsp-workspace-symbol)

;; Dash
(define-key leader-map "dap" 'dash-at-point)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
