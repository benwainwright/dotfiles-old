;;; editor/multiple-cursors/config.el -*- lexical-binding: t; -*-

(use-package! evil-mc
  :when (featurep! :editor evil)
  :commands (evil-mc-make-cursor-here
             evil-mc-make-all-cursors
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p
             evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-skip-and-goto-prev-match)
  :init
  (defvar evil-mc-key-map (make-sparse-keymap))
  :config
  (global-evil-mc-mode +1)
  (setq evil-mc-enable-bar-cursor (not (or IS-MAC IS-WINDOWS)))

  (after! smartparens
    ;; Make evil-mc cooperate with smartparens better
    (let ((vars (cdr (assq :default evil-mc-cursor-variables))))
      (unless (memq (car sp--mc/cursor-specific-vars) vars)
        (setcdr (assq :default evil-mc-cursor-variables)
                (append vars sp--mc/cursor-specific-vars)))))

  ;; Add custom commands to whitelisted commands
  (dolist (fn '(doom/backward-to-bol-or-indent doom/forward-to-last-non-comment-or-eol
                doom/backward-kill-to-bol-and-indent delete-char))
    (add-to-list 'evil-mc-custom-known-commands `(,fn (:default . evil-mc-execute-default-call-with-count))))
  ;; Have evil-mc work with explicit `evil-escape' (typically bound to C-g)
  (add-to-list 'evil-mc-custom-known-commands '(evil-escape (:default . evil-mc-execute-default-evil-normal-state)))

  ;; Activate evil-mc cursors upon switching to insert mode
  (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors)

  ;; disable evil-escape in evil-mc; causes unwanted text on invocation
  (add-to-list 'evil-mc-incompatible-minor-modes 'evil-escape-mode nil #'eq)

  (add-hook! 'doom-escape-hook
    (defun +multiple-cursors-escape-multiple-cursors-h ()
      "Clear evil-mc cursors and restore state."
      (when (evil-mc-has-cursors-p)
        (evil-mc-undo-all-cursors)
        (evil-mc-resume-cursors)
        t)))

  ;; Forward declare these so that ex completion and evil-mc support is
  ;; recognized before the autoloaded functions are loaded.
  (evil-add-command-properties '+evil:align :evil-mc t)
  (evil-add-command-properties '+multiple-cursors:evil-mc :evil-mc t))


(after! multiple-cursors-core
  (setq mc/list-file (concat doom-etc-dir "mc-lists.el"))

  ;; TODO multiple-cursors config for Emacs users?

  ;; mc doesn't play well with evil, this attempts to assuage some of its
  ;; problems so that any plugins that depend on multiple-cursors (which I have
  ;; no control over) can still use it in relative safety.
  (when (featurep! :editor evil)
    (evil-define-key* '(normal emacs) mc/keymap [escape] #'mc/keyboard-quit)

    (defvar +mc--compat-evil-prev-state nil)
    (defvar +mc--compat-mark-was-active nil)

    (add-hook! 'multiple-cursors-mode-enabled-hook
      (defun +multiple-cursors-compat-switch-to-emacs-state-h ()
        (when (and (bound-and-true-p evil-mode)
                   (not (memq evil-state '(insert emacs))))
          (setq +mc--compat-evil-prev-state evil-state)
          (when (region-active-p)
            (setq +mc--compat-mark-was-active t))
          (let ((mark-before (mark))
                (point-before (point)))
            (evil-emacs-state 1)
            (when (or +mc--compat-mark-was-active (region-active-p))
              (goto-char point-before)
              (set-mark mark-before))))))

    (add-hook! 'multiple-cursors-mode-disabled-hook
      (defun +multiple-cursors-compat-back-to-previous-state-h ()
        (when +mc--compat-evil-prev-state
          (unwind-protect
              (case +mc--compat-evil-prev-state
                ((normal visual) (evil-force-normal-state))
                (t (message "Don't know how to handle previous state: %S"
                            +mc--compat-evil-prev-state)))
            (setq +mc--compat-evil-prev-state nil)
            (setq +mc--compat-mark-was-active nil)))))

    ;; When running edit-lines, point will return (position + 1) as a result of
    ;; how evil deals with regions
    (defadvice! +multiple--cursors-adjust-mark-for-evil-a (&rest _)
      :before #'mc/edit-lines
      (when (and (bound-and-true-p evil-mode)
                 (not (memq evil-state '(insert emacs))))
        (if (> (point) (mark))
            (goto-char (1- (point)))
          (push-mark (1- (mark))))))

    (add-hook! 'rectangular-region-mode-hook
      (defun +multiple-cursors-evil-compat-rect-switch-state-h ()
        (if rectangular-region-mode
            (+multiple-cursors-compat-switch-to-emacs-state-h)
          (setq +mc--compat-evil-prev-state nil))))

    (defvar mc--default-cmds-to-run-once nil)))
