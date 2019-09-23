;;; emacs/vc/config.el -*- lexical-binding: t; -*-

(when IS-WINDOWS
  (setenv "GIT_ASKPASS" "git-gui--askpass"))


;;;###package vc
(setq vc-make-backup-files nil
      vc-follow-symlinks t)


(after! vc-annotate
  (set-popup-rules!
    '(("^\\vc-d" :select nil) ; *vc-diff*
      ("^\\vc-c" :select t))) ; *vc-change-log*
  (set-evil-initial-state!
    '(vc-annotate-mode vc-git-log-view-mode)
    'normal))


(after! git-timemachine
  ;; Sometimes I forget `git-timemachine' is enabled in a buffer, so instead of
  ;; showing revision details in the minibuffer, show them in
  ;; `header-line-format', which has better visibility.
  (setq git-timemachine-show-minibuffer-details t)
  (advice-add #'git-timemachine--show-minibuffer-details
              :override #'+vc-update-header-line-a)

  (after! evil
    ;; rehash evil keybindings so they are recognized
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

  (when (featurep! :tools magit)
    (add-transient-hook! #'git-timemachine-blame (require 'magit-blame)))

  (map! :map git-timemachine-mode-map
        :n "gtc" #'git-timemachine-show-commit))


(use-package! git-commit
  :after-call after-find-file
  :config
  (global-git-commit-mode +1)
  (set-yas-minor-mode! 'git-commit-mode)

  ;; Enforce git commit conventions.
  ;; See https://chris.beams.io/posts/git-commit/
  (setq git-commit-summary-max-length 50
        git-commit-style-convention-checks '(overlong-summary-line non-empty-second-line))
  (setq-hook! 'git-commit-mode-hook fill-column 72)

  (add-hook! 'git-commit-setup-hook
    (defun +vc-start-in-insert-state-maybe ()
      "Start git-commit-mode in insert state if in a blank commit message,
otherwise in default state."
      (when (and (bound-and-true-p evil-mode)
                 (bobp) (eolp))
        (evil-insert-state)))))


(after! smerge-mode
  (unless EMACS26+
    (with-no-warnings
      (defalias #'smerge-keep-upper #'smerge-keep-mine)
      (defalias #'smerge-keep-lower #'smerge-keep-other)
      (defalias #'smerge-diff-base-upper #'smerge-diff-base-mine)
      (defalias #'smerge-diff-upper-lower #'smerge-diff-mine-other)
      (defalias #'smerge-diff-base-lower #'smerge-diff-base-other))))

