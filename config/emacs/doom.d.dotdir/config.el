;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Fira Code" :size 15))

(setq initial-frame-alist
'((width . 106)
    (height . 60)))

(setq default-frame-alist
'((width . 106)
    (height . 60)))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(require 'filenotify)
(defun set-projectile-workspaces (&rest workspaces)
  (dolist (workspace workspaces)
    (projectile-discover-projects-in-directory workspace)
    (file-notify-add-watch workspace
                           '(change attribute-change)
                           (lambda (event)
                             (projectile-discover-projects-in-directory
                              (f-dirname (car (cdr (cdr event)))))))))

(set-projectile-workspaces "~/workspace" "~/repos")
(add-to-list 'projectile-known-projects "~/dotfiles")
