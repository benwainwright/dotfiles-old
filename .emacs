(setq initial-frame-alist
      '((tool-bar-lines . 0)
	(width . 106)
	(font . "Fira Code-13")
	(height . 60)
	(background-mode . 'dark)
	(ns-transparent-titlebar . t)
	(ns-appearance . dark)
	))

(setq default-frame-alist
      '((tool-bar-lines . 0)
	(font . "Fira Code-13")
	(width . 106)
	(height . 60)
	(background-mode . 'dark)
	(ns-transparent-titlebar . t)
	(ns-appearance . dark)

	))

(setq backup-directory-alist `(("." . "~/.emacssaves")))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq visible-bell t)
(setq custom-file "~/.emacs.d/custom.el")

(load custom-file)
(global-hl-line-mode +1)

(add-to-list 'load-path "~/.emacs.d/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(setq exec-path (append exec-path '("~/.emacs.d/tern/bin/")))

(defvar org-agenda-files)
(setq org-agenda-files (list "~/org/work.org"))

;; Enable ligatures
(when (window-system)
  (set-frame-font "Fira Code"))
(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
	       (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
	       (36 . ".\\(?:>\\)")
	       (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
	       (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
	       (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
	       (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
	       (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
	       (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
	       (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
	       (48 . ".\\(?:x[a-zA-Z]\\)")
	       (58 . ".\\(?:::\\|[:=]\\)")
	       (59 . ".\\(?:;;\\|;\\)")
	       (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
	       (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
	       (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
	       (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
	       (91 . ".\\(?:]\\)")
	       (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
	       (94 . ".\\(?:=\\)")
	       (119 . ".\\(?:ww\\)")
	       (123 . ".\\(?:-\\)")
	       (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
	       (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
	       )
	     ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
			  `([,(cdr char-regexp) 0 font-shape-gstring]))))

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

(global-git-gutter-mode +1)

(use-package diminish
  :ensure t
  :config
  (diminish 'git-gutter-mode)
  )

(use-package general
  :ensure t)

(use-package git-gutter
  :ensure t)

(use-package ag
  :ensure t)

(use-package linum-relative
  :ensure t)

(use-package xref-js2
  :ensure t)

(use-package feature-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package helm
  :ensure t
  :config
  (defvar helm-split-window-inside-p)
  (setq helm-split-window-inside-p t)
  (use-package helm-ag
    :ensure t))

(use-package rainbow-delimiters
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t)

(use-package org
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package projectile
  :ensure t)

(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match nil))

(use-package magit
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package js2-refactor
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (use-package evil-magit
    :ensure t))

(use-package groovy-mode
  :ensure t)

(use-package helm-config)

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

(use-package discover
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))	
    (exec-path-from-shell-initialize)))

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
  (use-package company-tern	
    :ensure	
    :config	
    (defvar company-backends)	
    (setq company-backends '(company-tern))))	

;; Allow hash to be entered	
(defun insert-pound ()	
  "Insert a pound into the buffer."	
  (insert "#"))	
(global-set-key (kbd "M-3") '(lambda()(interactive)(insert-pound)))

;; Keyboard mappings
(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil))

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(global-set-key (kbd "C-n") 'next-buffer)
(global-set-key (kbd "C-p") 'previous-buffer)
(global-set-key (kbd "s-f") 'helm-projectile-find-file-dwim)
(global-set-key (kbd "s-p") 'helm-projectile-switch-project)
(global-set-key (kbd "s-g") 'helm-projectile-ag)
(global-set-key (kbd "s-c") 'projectile-compile-project)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'javascript-mode (lambda () (tern-mode t)))
