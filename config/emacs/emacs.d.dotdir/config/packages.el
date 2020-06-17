;; Self contained packages file. Will install use-package if it isn't available
;; Then install all packages defined in this file
(provide 'packages)
(require 'package)
(package-initialize)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use-package definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package moe-theme
  :ensure t
  :config
  (load-theme 'moe-dark t))

;; Vim emulation
(use-package evil
  :ensure t
  :config
  (evil-mode 1))

;; Project management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

;; Fuzzy completion framework
(use-package helm
  :ensure t)

;; Sidebar
(use-package treemacs
  :ensure t)


(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config

  (require 'filenotify)
  (defun set-projectile-workspaces (&rest workspaces)
    (dolist (workspace workspaces)
      (projectile-discover-projects-in-directory workspace)
      (file-notify-add-watch workspace
			     '(change attribute-change)
			     (lambda (event)
			       (projectile-discover-projects-in-directory
				(f-dirname (car (cdr (cdr event)))))))))

  ;; Folders in this directory will be collected and added
  ;; to projectile projects every time the directories change

  (set-projectile-workspaces
   "~/workspace"
   "~/repos"
   "~/go/src/github.com/benwainwright/"
   "~/go/src/github.com/bbc/")

  (add-to-list 'projectile-known-projects "~/dotfiles")
  (helm-projectile-on))

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (add-hook 'web-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package magit
  :ensure t)

(use-package forge
  :ensure t
  :after magit)

(use-package evil-magit
  :ensure t
  :after (magit evil))

(use-package typescript-mode
  :ensure t)

(use-package format-all
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-backends (remove 'company-gtags company-backends))
  (setq company-backends (remove 'company-etags company-backends))
  (setq company-minimum-prefix-length 0)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (setq lsp-prefer-capf t)
  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 100000000)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))


(use-package flycheck
  :ensure t)

(use-package lsp-ui
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :config
  (lsp-treemacs-sync-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package powerline
  :ensure t)


(use-package git-gutter-fringe
  :ensure t)

(use-package helm-rg
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx" . web-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package browse-at-remote
  :ensure t)

(use-package ace-popup-menu
  :ensure t
  :config
  (ace-popup-menu-mode 1))

(use-package helpful
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))
