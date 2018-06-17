(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '((tool-bar-lines . 0)
              (width . 106)
              (height . 60)
              ))

      (setq default-frame-alist
            '((tool-bar-lines . 0)
	      (font . "FuraCode Nerd Font Mono-14")
              (width . 106)
              (height . 60)
              )))
  (progn
    (setq initial-frame-alist
          '((tool-bar-lines . 0)))
    (setq default-frame-alist
          '((tool-bar-lines . 0)))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ns-use-srgb-colorspace nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'load-path "~/.emacs.d/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(setq exec-path (append exec-path '("~/.emacs.d/tern/bin/")))

;; Load package management and install missing packages
(require 'package)
(defvar package-list)
(setq package-list
      '(
	json-mode
	editorconfig
	feature-mode
	magit
	exec-path-from-shell
	use-package
	evil
	ag
	helm
	helm-ag
	js2-mode
	js2-refactor
	xref-js2
	monokai-theme
	dracula-theme
	linum-relative
	org
	powerline
	rainbow-delimiters
	projectile
	helm-projectile
	flycheck
	company
	company-tern
	general
	))
(setq package-archives '(("melpa"        . "http://melpa.org/packages/")
			 ("MELPA stable" . "https://stable.melpa.org/packages/")
			 ("gnu"          . "http://elpa.gnu.org/packages/")
			 ("marmalade"    . "http://marmalade-repo.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Load and setup installed packages
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package linum-relative
  :config
  (linum-relative-global-mode))

(use-package helm-config)

(use-package powerline
  :config
  (powerline-default-theme))

(use-package flycheck
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(load-theme 'dracula t)

;; Allow hash to be entered
(defun insert-pound ()
  "Insert a pound into the buffer."
  (insert "#"))
(global-set-key (kbd "M-3") '(lambda()(interactive)(insert-pound)))

(defvar company-backends)
(setq company-backends '(company-tern))

;; Keyboard mappings
(global-unset-key (kbd "C-n"))
(global-set-key (kbd "C-n") 'next-buffer)
(global-unset-key (kbd "C-p"))
(global-set-key (kbd "C-p") 'previous-buffer)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'javascript-mode (lambda () (tern-mode t)))
