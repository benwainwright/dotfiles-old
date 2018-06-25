(setq initial-frame-alist
      '((tool-bar-lines . 0)
	(width . 106)
	(font . "Fira Code-13")
	(height . 60)
	(background-mode . 'dark)
	))

(setq default-frame-alist
      '((tool-bar-lines . 0)
	(font . "Fira Code-13")
	(width . 106)
	(height . 60)
	(background-mode . 'dark)
	))
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

;; Load package management and install missing packages
(require 'package)
(defvar package-list)
(setq package-list
      '(
	json-mode
	feature-mode
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

(global-set-key (kbd "C-n") 'next-buffer)
(global-set-key (kbd "C-p") 'previous-buffer)
(global-set-key (kbd "s-f") 'helm-projectile-find-file-dwim)
(global-set-key (kbd "s-p") 'helm-projectile-switch-project)
(global-set-key (kbd "s-g") 'helm-projectile-grep)
(global-set-key (kbd "s-c") 'projectile-compile-project)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'javascript-mode (lambda () (tern-mode t)))
