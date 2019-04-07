;;; cosmos.el --- API for Cosmos

;; Package-Requires ((request "20181129.1138"))
;;; Code:
(defvar cosmos-api-url "https://cosmos.api.bbci.co.uk/v1")

(require 'request)

(setq lexical-binding t)

(custom-set-variables '(request-log-level 'error)
  '(request-message-level 'error))

(defun api-request (url complete &optional error)
  "Send request to the Cosmos API to PATH with callback COMPLETE.
Optionally supply an ERROR callback function"
  (message (concat "calling api-request at " url))
  (request url
    :type "GET"
    :parser 'json-read
    :success (cl-function
               (lambda (&key data response &allow-other-keys)
                 (message "Response received")
                 (funcall complete data)))
    :error (cl-function
             (lambda (&rest args &key error-thrown &allow-other-keys)
               (funcall error error-thrown)))))

(defun cosmos-get-resource (done data resource &optional error)
  (if (and
        (listp (cdr (assoc resource data)))
        (assoc 'ref (assoc resource data)))
    (api-request
      (if (eq data nil)
        cosmos-api-url
        (cdr (assoc 'ref (assoc resource data))))
      (lambda (request-data)
        (cosmos-get-resource done request-data resource))
      (lambda (request-error)
        (message "Error while calling Cosmos API: %S" request-error)))
    (funcall done (assoc resource data))))


(defun counsel-cosmos ()
  (interactive)
  (api-request
    cosmos-api-url
    (lambda (data)
      (ivy-read "Resource: " (mapcar 'car data)))))


;; (api-request
;;   cosmos-api-url
;;   (lambda (request-data)
;;     (cosmos-get-resource
;;       (lambda (final-data)
;;         (cosmos-get-resource
;;           (lambda (more-data)
;;             (message (pp more-data)))
;;           final-data
;;           'ssh_keys))
;;       request-data
;;       'resources)))

(defun cosmos-list-resource (done &optional data error)
  (if (and data (eq (assoc 'ref data) nil))
    (funcall done (mapcar 'car data))
    (api-request
      (if (eq data nil)
        cosmos-api-url
        (cdr (assoc 'ref data)))
      (lambda (request-data)
        (cosmos-list-resource done request-data))
      (lambda (request-error)
        (message "Error while calling Cosmos API: %S" request-error)))))

(provide 'cosmos)
;;; cosmos.el ends here
