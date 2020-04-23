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

;; Populate the exec-path from shell $PATH
(use-package exec-path-from-shell
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

(use-package helm-rg
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
  (helm-projectile-on))

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
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

(use-package company
  :ensure t
  :config
  (setq company-backends (remove 'company-gtags company-backends))
  (setq company-backends (remove 'company-etags company-backends))
  (setq company-minimum-prefix-length 0)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 1)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package format-all
  :ensure t)

;; (use-package telephone-line
;;   :ensure t
;;   :config
;;   (telephone-line-mode 1))

(use-package moe-theme
  :ensure t
  :config
  (moe-dark)
  (set-face-attribute
   'fringe nil
   :foreground (face-foreground 'default)
   :background (face-background 'default))

  (set-face-attribute
   'line-number nil
   :background (face-background 'default))

  (set-face-attribute
   'line-number-current-line nil
   :background "#303030"
   :foreground (face-attribute 'line-number :foreground)))

(use-package neotree
  :ensure t
  :config
  (defun neotree-project-dir-toggle ()
    "Open NeoTree using the project root, using find-file-in-project,
or the current buffer directory."
    (interactive)
    (let ((project-dir
           (ignore-errors
           ;;; Pick one: projectile or find-file-in-project
             (projectile-project-root)
             ))
          (file-name (buffer-file-name))
          (neo-smart-open t))
      (if (and (fboundp 'neo-global--window-exists-p)
               (neo-global--window-exists-p))
          (neotree-hide)
        (progn
          (neotree-show)
          (if project-dir
              (neotree-dir project-dir))
          (if file-name
              (neotree-find file-name)))))))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package counsel
  :ensure t)

(use-package counsel-projectile
  :ensure t)

(use-package browse-at-remote
  :ensure t)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package lsp-mode
  :ensure t
  :hook (typescript-mode . lsp)
  :hook (web-mode . lsp)
  :commands lsp
  :config
  (setq company-lsp-cache-candidates t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language Specific Packages ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package jest
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package npm-mode
  :ensure t
  :config
  (npm-global-mode))

(use-package typescript-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

(use-package js2-mode
  :ensure t)

(use-package lua-mode
  :ensure t)


(use-package virtualenvwrapper
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package tide
  :ensure t
  :after (company web-mode)
  :config

  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (when
                  (string-equal
                   "tsx"
                   (file-name-extension buffer-file-name))
 		(setup-tide-mode))))

  (flycheck-add-mode 'typescript-tslint 'web-mode))
