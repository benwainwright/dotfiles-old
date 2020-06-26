(provide 'hooks)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'format-all-mode)
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'js-mode-hook (lambda () (flycheck-add-next-checker 'lsp 'javascript-eslint)))
(add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'lsp 'javascript-eslint)))
(add-hook 'js-mode-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'jest)))
(add-hook 'typescript-mode-hook (lambda () (flycheck-add-next-checker 'javascript-eslint 'jest)))
