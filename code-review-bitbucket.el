;;; code-review-bitbucket.el --- Bitbucket API -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Bitbucket API
;;
;;; Code:

(require 'code-review-db)

(defcustom code-review-bitbucket-host "api.bitbucket.org/2.0"
  "Host used to access Bitbucket API."
  :type 'string
  :group 'code-review)

(defclass code-review-bitbucket-repo (code-review-db-pullreq)
  ((callback            :initform nil)))

(cl-defmethod code-review-core-pullreq-diff ((bitbucket code-review-bitbucket-repo) callback)
  "Get PR diff from BITBUCKET, run CALLBACK."
  (let ((diff-url (let-alist (ghub-request "GET" (format "/repositories/%s/%s/pullrequests/%s"
                                                         (oref bitbucket owner)
                                                         (oref bitbucket repo)
                                                         (oref bitbucket number))
                                           nil
                                           :forge 'bitbucket
                                           :auth 'code-review
                                           :host code-review-bitbucket-host)
                    (-> .links.diff.href
                        (split-string code-review-bitbucket-host)
                        (-second-item)))))
    (ghub-request "GET" diff-url nil
                  :forge 'bitbucket
                  :auth 'code-review
                  :host code-review-bitbucket-host
                  :callback callback)))

(cl-defmethod code-review-core-diff-deferred ((bitbucket code-review-bitbucket-repo))
  "Get PR diff from BITBUCKET using deferred lib."
  (let ((d (deferred:new #'identity)))
    (code-review-core-pullreq-diff
     bitbucket
     (apply-partially
      (lambda (d v &rest _)
        (deferred:callback-post d v))
      d))
    d))

(cl-defmethod code-review-core-pullreq-infos ((bitbucket code-review-bitbucket-repo) callback)
  "Get PR infos from BITBUCKET, run CALLBACK."
  ;; TODO: implement infos
  (funcall callback))

(cl-defmethod code-review-core-infos-deferred ((bitbucket code-review-bitbucket-repo))
  "GET PR infos from BITBUCKET using deferred lib."
  (let ((d (deferred:new #'identity)))
    (code-review-core-pullreq-infos
     bitbucket
     (apply-partially
      (lambda (d v &rest _)
        (deferred:callback-post d v))
      d))
    d))

(provide 'code-review-bitbucket)
;;; code-review-bitbucket.el ends here
