;; Allow hash to be entered
(defun insert-pound ()
  "Insert a pound into the buffer."
  (insert "#"))
(global-set-key (kbd "M-3") '(lambda()(interactive)(insert-pound)))
