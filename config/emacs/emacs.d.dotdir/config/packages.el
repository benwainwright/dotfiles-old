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
   "~/go/src/github.com/benwainwright/")

  (add-to-list 'projectile-known-projects "~/dotfiles")
  (helm-projectile-on))

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t
  :after (magit evil))

(use-package company
  :ensure t
  :config
  (setq company-minimum-prefix-length 0)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package format-all
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language Specific Packages ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package go-mode
  :ensure t)
