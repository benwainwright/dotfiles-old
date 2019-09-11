(setq initial-frame-alist
'
  (
    (tool-bar-lines . 0)
    (width . 106)
    (font . "Fira Code-15")
    (height . 60)
    (background-mode . 'dark)
    (ns-transparent-titlebar . t)
    (ns-appearance . dark)))

(setq default-frame-alist
'
  (
    (tool-bar-lines . 0)
    (font . "Fira Code-15")
    (width . 106)
    (height . 60)
    (background-mode . 'dark)
    (ns-transparent-titlebar . t)
    (ns-appearance . dark)))

(setq telephone-line-height 18)

(use-package monokai-theme
:ensure t
:config
  (load-theme 'monokai t))


;; Enable ligatures
(when
  (window-system)
  (set-frame-font "Fira Code"))
(let
  (
    (alist '
      (
        (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
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
)))
  (dolist
    (char-regexp alist)
    (set-char-table-range composition-function-table
      (car char-regexp)
`
      ([,
        (cdr char-regexp) 0 font-shape-gstring]))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:


