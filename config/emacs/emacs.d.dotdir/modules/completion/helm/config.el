;;; completion/helm/config.el -*- lexical-binding: t; -*-

(defvar +helm-project-search-engines '(rg ag pt)
  "What search tools for `+helm/project-search' (and `+helm-file-search' when no
ENGINE is specified) to try, and in what order.

To disable a particular tool, remove it from this list. To prioritize a tool
over others, move it to the front of the list. Later duplicates in this list are
silently ignored.

This falls back to git-grep (then grep) if none of these available.")

;; Posframe (requires +childframe)
(defvar +helm-posframe-handler #'+helm-poshandler-frame-center-near-bottom-fn
  "The function that determines the location of the childframe. It should return
a cons cell representing the X and Y coordinates. See
`posframe-poshandler-frame-center' as a reference.")

(defvar +helm-posframe-text-scale 1
  "The text-scale to use in the helm childframe. Set to nil for no scaling. Can
be negative.")

(defvar +helm-posframe-parameters
  '((internal-border-width . 8)
    (width . 0.5)
    (height . 0.35)
    (min-width . 80)
    (min-height . 16))
  "TODO")


;;
;;; Packages

(use-package! helm-mode
  :defer t
  :after-call pre-command-hook
  :init
  (map! [remap apropos]                   #'helm-apropos
        [remap find-library]              #'helm-locate-library
        [remap bookmark-jump]             #'helm-bookmarks
        [remap execute-extended-command]  #'helm-M-x
        [remap find-file]                 #'helm-find-files
        [remap locate]                    #'helm-locate
        [remap imenu]                     #'helm-semantic-or-imenu
        [remap noop-show-kill-ring]       #'helm-show-kill-ring
        [remap persp-switch-to-buffer]    #'+helm/workspace-mini
        [remap switch-to-buffer]          #'helm-buffers-list
        [remap projectile-find-file]      #'+helm/projectile-find-file
        [remap projectile-recentf]        #'helm-projectile-recentf
        [remap projectile-switch-project] #'helm-projectile-switch-project
        [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer
        [remap recentf-open-files]        #'helm-recentf
        [remap yank-pop]                  #'helm-show-kill-ring)
  :config
  (helm-mode +1)
  ;; helm is too heavy for `find-file-at-point'
  (add-to-list 'helm-completing-read-handlers-alist (cons #'find-file-at-point nil)))


(use-package! helm
  :after helm-mode
  :preface
  (setq helm-candidate-number-limit 50
        ;; Remove extraineous helm UI elements
        helm-display-header-line nil
        helm-mode-line-string nil
        helm-ff-auto-update-initial-value nil
        helm-find-files-doc-header nil
        ;; Don't override evil-ex's completion
        helm-mode-handle-completion-in-region nil
        ;; Default helm window sizes
        helm-display-buffer-default-width nil
        helm-display-buffer-default-height 0.25
        ;; When calling `helm-semantic-or-imenu', don't immediately jump to
        ;; symbol at point
        helm-imenu-execute-action-at-once-if-one nil
        ;; disable special behavior for left/right, M-left/right keys.
        helm-ff-lynx-style-map nil)

  (when (featurep! :editor evil +everywhere)
    (setq helm-default-prompt-display-function #'+helm--set-prompt-display))

  :init
  (when (and EMACS26+ (featurep! +childframe))
    (setq helm-display-function #'+helm-posframe-display-fn))

  (let ((fuzzy (featurep! +fuzzy)))
    (setq helm-M-x-fuzzy-match fuzzy
          helm-ag-fuzzy-match fuzzy
          helm-apropos-fuzzy-match fuzzy
          helm-apropos-fuzzy-match fuzzy
          helm-bookmark-show-location fuzzy
          helm-buffers-fuzzy-matching fuzzy
          helm-completion-in-region-fuzzy-match fuzzy
          helm-completion-in-region-fuzzy-match fuzzy
          helm-ff-fuzzy-matching fuzzy
          helm-file-cache-fuzzy-match fuzzy
          helm-flx-for-helm-locate fuzzy
          helm-imenu-fuzzy-match fuzzy
          helm-lisp-fuzzy-completion fuzzy
          helm-locate-fuzzy-match fuzzy
          helm-mode-fuzzy-match fuzzy
          helm-projectile-fuzzy-match fuzzy
          helm-recentf-fuzzy-match fuzzy
          helm-semantic-fuzzy-match fuzzy))

  :config
  (set-popup-rule! "^\\*helm" :vslot -100 :size 0.22 :ttl nil)

  ;; HACK Doom doesn't support these commands, which invite the user to install
  ;; the package via ELPA. Force them to use +helm/* instead, because they work
  ;; out of the box.
  (advice-add #'helm-projectile-rg :override #'+helm/rg)
  (advice-add #'helm-projectile-ag :override #'+helm/ag)
  (advice-add #'helm-projectile-grep :override #'+helm/grep)

  ;; Hide the modeline
  (defun +helm--hide-mode-line (&rest _)
    (with-current-buffer (helm-buffer-get)
      (unless helm-mode-line-string
        (hide-mode-line-mode +1))))
  (add-hook 'helm-after-initialize-hook #'+helm--hide-mode-line)
  (advice-add #'helm-display-mode-line :override #'+helm--hide-mode-line)
  (advice-add #'helm-ag-show-status-default-mode-line :override #'ignore)

  ;; Use helpful instead of describe-* to display documentation
  (dolist (fn '(helm-describe-variable helm-describe-function))
    (advice-add fn :around #'doom-use-helpful-a)))


(use-package! helm-flx
  :when (featurep! +fuzzy)
  :hook (helm-mode . helm-flx-mode)
  :config (helm-flx-mode +1))


;;;###package helm-ag
(after! helm-ag
  (map! :map helm-ag-edit-map :n "RET" #'compile-goto-error)
  (define-key helm-ag-edit-map [remap quit-window] #'helm-ag--edit-abort)
  (set-popup-rule! "^\\*helm-ag-edit" :size 0.35 :ttl 0 :quit nil)
  ;; Recenter after jumping to match
  (advice-add #'helm-ag--find-file-action :after-while #'doom-recenter-a)
  ;; And record position before jumping
  (advice-add #'helm-ag--find-file-action :around #'doom-set-jump-maybe-a))


;;;###package helm-bookmark
(setq helm-bookmark-show-location t)


;;;###package helm-files
(after! helm-files
  (setq helm-boring-file-regexp-list
        (append (list "\\.projects$" "\\.DS_Store$")
                helm-boring-file-regexp-list)))


;;;###package helm-locate
(defvar helm-generic-files-map (make-sparse-keymap))
(after! helm-locate
  (when (and IS-MAC
             (null helm-locate-command)
             (executable-find "mdfind"))
    (setq helm-locate-command "mdfind -name %s"))
  (set-keymap-parent helm-generic-files-map helm-map))


;;;###package helm-projectile
(use-package! helm-projectile
  :commands (helm-projectile-find-file
             helm-projectile-recentf
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (setq projectile-completion-system 'helm)
  (defvar helm-projectile-find-file-map (make-sparse-keymap))
  :config
  (set-keymap-parent helm-projectile-find-file-map helm-map))


;;;###package swiper-helm
(after! swiper-helm
  (setq swiper-helm-display-function
        (lambda (buf &optional _resume) (pop-to-buffer buf)))
  (global-set-key [remap swiper] #'swiper-helm)
  (add-to-list 'swiper-font-lock-exclude #'+doom-dashboard-mode nil #'eq))
