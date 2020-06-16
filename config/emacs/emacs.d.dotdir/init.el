(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/vendored")

(require 'hooks)
(require 'packages)
(require 'initial-frame)
(require 'ligatures)
(require 'settings)
(require 'maps)
(require 'mac)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-delimiters lsp-treemacs company-capf use-package typescript-mode treemacs telephone-line monokai-theme lsp-ui helm-projectile github-theme format-all forge flycheck exec-path-from-shell evil-magit editorconfig company-lsp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
