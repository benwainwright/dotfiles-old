;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Fira Code" :size 15))

(setq initial-frame-alist
'((width . 106)
    (height . 60)))

(setq default-frame-alist
'((width . 106)
    (height . 60)))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(require 'filenotify)
(defun set-projectile-workspaces (&rest workspaces)
  (dolist (workspace workspaces)
    (projectile-discover-projects-in-directory workspace)
    (file-notify-add-watch workspace
                           '(change attribute-change)
                           (lambda (event)
                             (projectile-discover-projects-in-directory
                              (f-dirname (car (cdr (cdr event)))))))))

(set-projectile-workspaces "~/workspace" "~/repos")
(add-to-list 'projectile-known-projects "~/dotfiles")

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(define-minor-mode my-global-keymaps-mode
  "Personal global keybindings"
  :keymap (make-sparse-keymap)
  :init-value t)


(after! evil
  (unbind-key "C-f" evil-normal-state-map)
  (unbind-key "C-f" evil-motion-state-map)
  (unbind-key "C-p" evil-normal-state-map)
  (unbind-key "C-p" evil-motion-state-map)
  (unbind-key "C-n" evil-normal-state-map)
  (unbind-key "C-n" evil-motion-state-map)
  (unbind-key "C-t" evil-normal-state-map)
  (unbind-key "C-t" evil-motion-state-map)
  (bind-key ";" 'helm-buffers-list evil-motion-state-map)
  (bind-key ";" 'helm-buffers-list evil-normal-state-map))

(after! tide
  (unbind-key "C-t" tide-mode-map))

(define-key my-global-keymaps-mode-map (kbd "C-n") 'next-buffer)
(define-key my-global-keymaps-mode-map (kbd "C-p") 'previous-buffer)
(define-key my-global-keymaps-mode-map (kbd "S-p") 'projectile-switch-project)
(define-key my-global-keymaps-mode-map (kbd "C-f") 'projectile-find-file)
(define-key my-global-keymaps-mode-map (kbd "S-c") 'projectile-compile-project)
(define-key my-global-keymaps-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key my-global-keymaps-mode-map (kbd "M-p") 'flycheck-previous-error)
(define-key my-global-keymaps-mode-map (kbd "C-j") 'windmove-down)
(define-key my-global-keymaps-mode-map (kbd "C-k") 'windmove-up)
(define-key my-global-keymaps-mode-map (kbd "C-h") 'windmove-left)
(define-key my-global-keymaps-mode-map (kbd "C-l") 'windmove-right)
(define-key my-global-keymaps-mode-map (kbd "C-e") 'neotree-projectile-action)

(my-global-keymaps-mode)

;(add-hook 'js2-mode-hook #'lsp)
;(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(setq display-line-numbers-type 'relative)
(define-key key-translation-map (kbd "M-3") (kbd "#"))

(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path))

(setq exec-path (append exec-path '("/usr/local/bin")))
(setq lsp-print-io t)
setq display-line-numbers 'relative)
