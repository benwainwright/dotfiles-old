;;; lang/markdown/config.el -*- lexical-binding: t; -*-

(defvar +markdown-compile-functions
  '(+markdown-compile-marked
    +markdown-compile-pandoc
    +markdown-compile-markdown
    +markdown-compile-multimarkdown)
  "A list of commands to try when attempting to build a markdown file with
`markdown-open' or `markdown-preview', stopping at the first one to return non-nil.

Each function takes three argument. The beginning position of the region to
capture, the end position, and the output buffer.")


;;
;;; Packages

(use-package! markdown-mode
  :mode ("/README\\(?:\\.\\(?:markdown\\|md\\)\\)?\\'" . gfm-mode)
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")
        markdown-fontify-code-blocks-natively t
        markdown-hide-urls nil ; trigger with `markdown-toggle-url-hiding'
        markdown-enable-math t ; syntax highlighting for latex fragments
        markdown-gfm-uppercase-checkbox t ; for compat with org-mode
        markdown-command #'+markdown-compile
        markdown-open-command
        (cond (IS-MAC "open")
              (IS-LINUX "xdg-open")))

  :config
  (set-flyspell-predicate! '(markdown-mode gfm-mode)
    #'+markdown-flyspell-word-p)
  (set-lookup-handlers! '(markdown-mode gfm-mode)
    :file #'markdown-follow-thing-at-point)

  ;; Enable hard-wrapping. By convention, Doom does this for all textual modes.
  (add-hook 'markdown-mode-hook #'auto-fill-mode)

  ;; Prevent mis-fontification of YAML metadata blocks in `markdown-mode' which
  ;; occurs when the first line contains a colon in it. See
  ;; https://github.com/jrblevin/markdown-mode/issues/328.
  (advice-add :markdown-match-generic-metadata
              :override #'+markdown-disable-front-matter-fontification-a)

  (map! :map markdown-mode-map
        :n [tab] #'markdown-cycle
        :n "TAB" #'markdown-cycle
        :n [backtab] #'markdown-shifttab
        :n "<S-tab>" #'markdown-shifttab
        :i "M-*" #'markdown-insert-list-item
        :i "M-b" #'markdown-insert-bold
        :i "M-i" #'markdown-insert-italic
        :i "M-`" #'+markdown/insert-del
        (:when (featurep! :editor evil +everywhere)
          :m "gj"  #'markdown-next-visible-heading
          :m "gk"  #'markdown-previous-visible-heading
          ;; TODO: Make context sensitive
          :m "]h"  #'markdown-next-visible-heading
          :m "[h"  #'markdown-previous-visible-heading
          :m "[p"  #'markdown-promote
          :m "]p"  #'markdown-demote
          :m "[l"  #'markdown-previous-link
          :m "]l"  #'markdown-next-link
          :i "M--" #'markdown-insert-hr
          :n "M-r" #'browse-url-of-file)
        (:localleader
          "o" #'markdown-open
          "b" #'markdown-preview
          (:prefix "i"
            "t" #'markdown-toc-generate-toc
            "i" #'markdown-insert-image
            "l" #'markdown-insert-link))))
