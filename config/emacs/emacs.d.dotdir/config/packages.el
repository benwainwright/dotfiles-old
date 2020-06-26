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

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (defvar doom-themes-treemacs-theme)
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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

(use-package helm-ag
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
  (add-to-list 'projectile-known-projects "~/Dropbox (BBC)/org")
  (helm-projectile-on))

(use-package lsp-mode
  :ensure t
  :config
  (setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
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
  :ensure t
  :config
  (setq magit-prefer-remote-upstream t))

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

(use-package pos-tip
  :ensure t)

(use-package company
  :ensure t
  :bind
  :config

  (defvar company-lsp-cache-candidates)
  (setq company-lsp-cache-candidates t)

  (setq company-backends (remove 'company-gtags company-backends))
  (setq company-backends (remove 'company-etags company-backends))
  (setq company-minimum-prefix-length 0)

  (defvar company-dabbrev-downcase)
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
  :ensure t
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint emacs-lisp-checkdoc)))


  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
			(expand-file-name "node_modules/eslint/bin/eslint.js"
					  root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

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

(use-package ag
  :ensure t)

;;(use-package ace-popup-menu
;;  :ensure t
;;  :config
;;  (ace-popup-menu-mode 1))

(use-package helpful
  :ensure t)

(use-package rainbow-mode
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package dimmer
  :ensure t
  :config
  (dimmer-configure-helm)
  (dimmer-configure-which-key)
  (dimmer-mode t))

(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-mode t))

(use-package string-inflection
  :ensure t
  :config

  ;; C-q C-u is the key bindings similar to Vz Editor.
  (global-unset-key (kbd "C-q"))

  (defun my-string-inflection-cycle-auto ()
    "switching by major-mode"
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     (t
      (string-inflection-java-style-cycle)))))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (evilnc-default-hotkeys))

(use-package smartparens
  :ensure t
  :config
  (progn (show-smartparens-global-mode t))
  (add-hook 'prog-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook #'turn-on-smartparens-mode))

(use-package guide-key
  :ensure t
  :config
  (guide-key-mode 1))

(use-package helm-descbinds
  :ensure t
  :config
  (helm-descbinds-mode))

(use-package google-this
  :ensure t
  :config
  (google-this-mode 1))

(use-package helm-lsp
  :ensure t)

(use-package flycheck-jest
  :ensure t
  :config
  (flycheck-jest-setup))

(use-package org
  :ensure t
  :config
  (setq org-directory "~/Dropbox (BBC)/org/")
  (setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                             ("~/gtd/someday.org" :level . 1)
                             ("~/gtd/tickler.org" :maxlevel . 2)))
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file+headline "~/Dropbox (BBC)/org/inbox.org" "Tasks")
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline "~/Dropbox (BBC)/org/tickler.org" "Tickler")
                                 "* %i%? \n %U")))

  (setq org-agenda-files '("~/Dropbox (BBC)/org/gtd.org"
                           "~/Dropbox (BBC)/org/inbox.org"
                           "~/Dropbox (BBC)/org/someday.org")))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package no-littering
  :ensure t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
