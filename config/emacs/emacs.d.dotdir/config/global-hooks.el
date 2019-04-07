(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
