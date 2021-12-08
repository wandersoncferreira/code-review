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
  (ghub-request "GET" (format "/repositories/%s/%s/pullrequests/%s"
                              (oref bitbucket owner)
                              (oref bitbucket repo)
                              (oref bitbucket number))
                nil
                :forge 'bitbucket
                :auth 'code-review
                :host code-review-bitbucket-host
                :callback callback))

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

(defclass code-review-submit-bitbucket-review ()
  ((state :initform nil)
   (pr :initform nil)
   (local-comments :initform nil
                   :type (satisfies
                          (lambda (it)
                            (-all-p #'code-review-submit-local-coment-p it))))
   (feedback :initform nil)))

(cl-defmethod code-review-core-send-review ((review code-review-submit-bitbucket-review) callback)
  "Submit review comments given REVIEW and a CALLBACK."
  (let* ((pr (oref review pr))
         (infos (oref pr raw-infos)))
    ;; 1. send all comments to the PR
    (dolist (c (oref review local-comments))
      (ghub-request "POST" (format "/repositories/%s/%s/pullrequests/%s/comments"
                                   (oref pr owner)
                                   (oref pr repo)
                                   (oref pr number))
                    nil
                    :forge 'bitbucket
                    :auth 'code-review
                    :host code-review-bitbucket-host
                    :payload `((content (raw . ,(oref c body)))
                               (inline (path . ,(oref c path))
                                       (from . 23))) ;;; TODO: compute the correct line number.. use the same strategy used in Gitlab integration
                    ))))

(provide 'code-review-bitbucket)
;;; code-review-bitbucket.el ends here
