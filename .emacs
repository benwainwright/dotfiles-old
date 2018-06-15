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

(setq ns-use-srgb-colorspace nil)

(setq package-list
      '(
	use-package
	evil
	helm
	js2-mode
	monokai-theme
	dracula-theme
	linum-relative
	org
	powerline
	rainbow-delimiters
	projectile
	helm-projectile
	flycheck
	))

(require 'package)

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

(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; Use `display-line-number-mode` as linum-mode's backend for smooth performance
(setq linum-relative-backend 'display-line-numbers-mode)

(require 'linum-relative)
(linum-on)

(require 'helm-config)
(set-face-attribute 'default nil :height 140)

(load-theme 'dracula t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'powerline)
(powerline-default-theme)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(linum-relative-mode)
