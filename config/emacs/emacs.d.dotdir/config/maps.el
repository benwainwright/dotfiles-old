(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(global-set-key (kbd "C-n") 'next-buffer)
(global-set-key (kbd "C-p") 'previous-buffer)
(global-set-key (kbd "s-p") 'counsel-projectile-switch-project)
(global-set-key (kbd "s-g") 'counsel-projectile-rg)
(global-set-key (kbd "s-c") 'projectile-compile-project)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-files)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
