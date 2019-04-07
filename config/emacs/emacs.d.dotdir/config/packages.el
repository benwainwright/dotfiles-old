;; packages.el --- package declaration and configuration
(require 'package)

(package-initialize)

(setq package-archives
      '(("melpa"        . "http://melpa.org/packages/")
        ("MELPA stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")))


(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package request
  :ensure t)

(use-package xterm-color
  :ensure t
  :config
  (require 'eshell)
  (add-hook 'eshell-before-prompt-hook
    (lambda ()
      (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  )

(use-package browse-kill-ring
  :ensure t)

(use-package smartparens
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package diminish
  :ensure t
  :config
  (diminish 'git-gutter-mode))

(use-package general
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(use-package ag
  :ensure t)

(use-package linum-relative
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package xref-js2
  :ensure t)

(use-package feature-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)


(use-package multi-term
  :ensure t
  :config
  (setq multi-term-program "/usr/local/bin/zsh")
  (add-hook 'term-mode-hook
	    (lambda ()
	      (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
	      (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))
  (add-hook 'term-mode-hook
	    (lambda ()
	      (define-key term-raw-map (kbd "C-y") 'term-paste))))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(defun refresh-projectile-projects()
  "Reload all projects from defined locations into projectile."
  (interactive)
  (setq projectile-known-projects ())
  (add-to-list 'projectile-known-projects "~/org")
  (add-to-list 'projectile-known-projects "~/dotfiles")
  (add-projects-in-path-to-projectile-known-projects "~/workspace")
  (add-projects-in-path-to-projectile-known-projects "~/repos"))

(defun add-projects-in-path-to-projectile-known-projects(path)
  "Load all top level directories from 'path' into projectile as known projects"
  (interactive "sPath: ")
  (dolist (entry (directory-files path t))
    (add-to-list 'projectile-known-projects entry)))

;; (use-package excorporate
;;   :ensure t
;;   :config
;;   (setq excorporate-configuration
;; 	'("ben.wainwright@bbc.co.uk" . "https://oa.myconnect.bbc.co.uk/ews/exchange.asmx")
;; 	org-agenda-include-diary t)
;;   (excorporate)
;;   (excorporate-diary-enable)
;;   )

(use-package projectile
  :ensure t
  :bind ("C-f" . helm-projectile-find-file)
  :config
  (refresh-projectile-projects))

(use-package helm-projectile
  :ensure t
  :after helm)

(use-package magit
  :ensure t
  :config
  (magit-save-repository-buffers 'dontask)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
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

(use-package evil-surround
  :ensure t
  :after evil)

(use-package evil-magit
  :ensure t
  :after (evil magit magit-popup))

(use-package evil-org
  :ensure t
  :after (org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package helm
  :config
  (require 'helm-config)
  (helm-mode)
  :ensure t)

(use-package helm-swoop
  :ensure t
  :config
  (bind-key "/" 'helm-swoop-from-evil-search evil-motion-state-map)
  :after (helm evil))

(use-package groovy-mode
    :ensure t)

(use-package telephone-line
  :ensure t
  :config
  (telephone-line-mode 1))

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

(use-package discover
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package eglot
  :ensure t
  :after projectile
  :config
  (defun me:project-finder (dir)
    (if (fboundp 'projectile-project-root)
	(let ((root (projectile-project-root dir)))
          (and root (cons 'transient root)))))
  (add-to-list 'project-find-functions #'me:project-finder)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure))

(use-package flycheck
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package quelpa
  :ensure t)

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://framagit.org/steckerhalter/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package quelpa-use-package
  :after quelpa
  :ensure t)

;; (use-package realgud-node-inspect
;;   :after realgud
;;   :quelpa (realgud-node-inspect
;;             :fetcher github
;;             :repo "realgud/realgud-node-inspect"))

(use-package realgud
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package f
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t)

(use-package org
  :ensure t)

(use-package restclient
  :ensure t)

(use-package org-bullets
  :after org
  :ensure t)

(use-package browse-at-remote
  :ensure t)

;; (use-package evil-leader
;;   :ensure t
;;   :after (evil browse-at-remote)
;;   :config
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader "<SPC>")
;;   (evil-leader/set-key
;;     "s" 'magit-status
;;     "r" 'reload-init-file
;;     "b" 'browse-at-remote
;;     "gcop" 'forge-checkout-pullreq
;;     "c" 'browse-at-remote)


(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)
