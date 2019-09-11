;-*- mode: Lisp; -*-
(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/emacs-personal-packages")

; Putting this right at the start so that no matter how badly
; I screw things up I can always reload easily
;(setq init-file-path load-file-name)
;(defun reload-init-file ()
;  (interactive)
;  (load-file init-file-path))

(load "packages")

; Packages must be loaded before visual, as the theme
; is loaded using use-package
(load "visual")
(load "settings")
;(load "org-settings")
;(load "1pass")
(load "global-hooks")
(load "mac")
(load "maps")
;(load "cosmos")
;(load "eglot-flycheck")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (solarized-theme zenburn-theme yasnippet-snippets xterm-color xref-js2 which-key web-mode typescript-mode treemacs-projectile treemacs-evil telephone-line symon smartparens smart-mode-line rjsx-mode restclient request realgud-node-inspect rainbow-delimiters quelpa-use-package powerline plantuml-mode org-bullets multi-term monokai-theme material-theme magit-popup lua-mode linum-relative json-mode jenkins helm-system-packages helm-swoop helm-rg helm-projectile groovy-mode git-gutter general format-all forge focus flycheck feature-mode exec-path-from-shell evil-surround evil-org evil-magit evil-leader eglot editorconfig drag-stuff dracula-theme dockerfile-mode docker-compose-mode discover diminish diff-hl deadgrep dashboard dash-at-point darkroom counsel-projectile company browse-kill-ring browse-at-remote aggressive-indent ag ace-popup-menu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
