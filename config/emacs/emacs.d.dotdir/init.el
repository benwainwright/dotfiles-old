;-*- mode: Lisp; -*-
(add-to-list 'load-path "~/.emacs.d/config")

; Putting this right at the start so that no matter how badly
; I screw things up I can always reload easily
(setq init-file-path load-file-name)
(defun reload-init-file ()
  (interactive)
  (load-file init-file-path))

(load "visual")
(load "settings")
(load "org-settings")
(load "packages")
(load "global-hooks")
(load "mac")
(load "maps")
