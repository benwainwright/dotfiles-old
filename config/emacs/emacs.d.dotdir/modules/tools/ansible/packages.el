;; -*- no-byte-compile: t; -*-
;;; tools/ansible/packages.el

(package! yaml-mode)
(package! ansible :recipe (:nonrecursive t))
(package! ansible-doc)
(package! jinja2-mode)

(when (featurep! :completion company)
  (package! company-ansible))
