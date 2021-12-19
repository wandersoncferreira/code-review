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

(cl-defmethod code-review-pullreq-diff ((bitbucket code-review-bitbucket-repo) callback)
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

(cl-defmethod code-review-diff-deferred ((bitbucket code-review-bitbucket-repo))
  "Get PR diff from BITBUCKET using deferred lib."
  (let ((d (deferred:new #'identity)))
    (code-review-pullreq-diff
     bitbucket
     (apply-partially
      (lambda (d v &rest _)
        (deferred:callback-post d v))
      d))
    d))

(cl-defmethod code-review-pullreq-infos ((bitbucket code-review-bitbucket-repo) callback)
  "Get PR infos from BITBUCKET, run CALLBACK."
  (let* ((res (ghub-request "GET" (format "/repositories/%s/%s/pullrequests/%s"
                                          (oref bitbucket owner)
                                          (oref bitbucket repo)
                                          (oref bitbucket number))
                            nil
                            :forge 'bitbucket
                            :auth 'code-review
                            :host code-review-bitbucket-host))
         (comments (->> (ghub-request "GET" (format "/repositories/%s/%s/pullrequests/%s/comments?q=deleted=false&size=100"
                                                    (oref bitbucket owner)
                                                    (oref bitbucket repo)
                                                    (oref bitbucket number))
                                      nil
                                      :forge 'bitbucket
                                      :auth 'code-review
                                      :host code-review-bitbucket-host)
                        (-remove
                         (lambda (it)
                           (a-get it 'deleted)))))
         (top-level-comments (->> comments
                                  (-remove
                                   (lambda (it)
                                     (a-get it 'inline)))
                                  (-map
                                   (lambda (it)
                                     (let-alist it
                                       `((bodyHTML . ,.content.html)
                                         (createdAt . ,.created_on)
                                         (reactions)
                                         (author (login . ,.user.nickname))
                                         (databaseId . ,.id)
                                         (typename . ,.type)))))))
         (diff-comments (->> comments
                             (-filter
                              (lambda (it)
                                (a-get it 'inline)))
                             (-map
                              (lambda (it)
                                (let-alist it
                                  (let ((position-fixed (or .inline.from
                                                            .inline.to))
                                        (position-type (if .inline.from
                                                           :from
                                                         :to)))
                                    `((typename . ,.type)
                                      (author (login . ,.user.nickname))
                                      (databaseId . ,.id)
                                      (state . "COMMENTED")
                                      (position . ,position-fixed)
                                      (position-type . ,position-type)
                                      (path . ,.inline.path)
                                      (comments (nodes ((bodyHTML . ,.content.html)
                                                        (originalPosition . ,position-fixed)
                                                        (position-type . ,position-type)
                                                        (path . ,.inline.path)
                                                        (createdAt . ,.created_on))))))))))))
    (funcall callback (-> res
                          (a-assoc-in (list 'comments 'nodes) top-level-comments)
                          (a-assoc-in (list 'reviews 'nodes) diff-comments)))))

(cl-defmethod code-review-infos-deferred ((bitbucket code-review-bitbucket-repo))
  "GET PR infos from BITBUCKET using deferred lib."
  (let ((d (deferred:new #'identity)))
    (code-review-pullreq-infos
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

(cl-defmethod code-review-send-review ((review code-review-submit-bitbucket-review) callback)
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

;;; fixes

(defvar code-review-bitbucket-line-diff-mapping nil
  "Hold structure to convert Line number position into diff positions.
For internal usage only.")

(defun code-review-bitbucket-pos-line-number->diff-line-number (bitbucket-diff-raw)
  "Get mapping of pos-line to diff-line given BITBUCKET-DIFF-RAW."
  (let* ((if-zero-null (lambda (n)
                         (let ((nn (string-to-number n)))
                           (when (> nn 0)
                             nn))))
         (bitbucket-diff
          (cdr (split-string bitbucket-diff-raw "diff --git")))
         (regex
          (rx "--- a/"
              (group-n 1 (+? anything))
              "+++ b/"
              (group-n 2 (+? anything))
              "@@ -"
              (group-n 3 (one-or-more digit))
              ","
              (group-n 4 (one-or-more digit))
              " +"
              (group-n 5 (one-or-more digit))
              ","
              (group-n 6 (one-or-more digit))))
         (res
          (-reduce-from
           (lambda (acc it)
             (save-match-data
               (if (string-match regex it)
                   (let* ((path-1 (match-string 1 it))
                          (path-2 (match-string 2 it))
                          (path (if (string-equal path-1 "/dev/null")
                                    path-2
                                  path-1)))
                     (a-assoc acc (string-trim path)
                              (a-alist 'old (a-alist 'beg (funcall if-zero-null (match-string 3 it))
                                                     'end (funcall if-zero-null (match-string 4 it))
                                                     'path path-1)
                                       'new (a-alist 'beg (funcall if-zero-null (match-string 5 it))
                                                     'old (funcall if-zero-null (match-string 6 it))
                                                     'path path-2))))
                 acc)))
           nil bitbucket-diff)))
    (setq code-review-bitbucket-line-diff-mapping res)))

(defun code-review-bitbucket--fix-diff-comments (raw-infos)
  "Fix RAW-INFOS diff comments position to be diff-pos instead of line number."
  (let ((diff-comments
         (-map
          (lambda (it)
            (let-alist it
              (let* ((mapping (alist-get (string-trim .path) code-review-bitbucket-line-diff-mapping
                                         nil nil 'equal))
                     (diff-pos
                      (cond
                       ((eq :to .position-type)
                        (- .position
                           (a-get-in mapping (list 'old 'beg))
                           1))
                       ((eq :from .position-type)
                        (+ 1 (- .position
                                (a-get-in mapping (list 'old 'beg)))))))
                     (cn (a-assoc (-first-item .comments.nodes) 'originalPosition diff-pos)))
                (-> it
                    (a-assoc 'position diff-pos)
                    (a-assoc-in (list 'comments 'nodes) (list cn))))))
          (a-get-in raw-infos (list 'reviews 'nodes)))))
    (a-assoc-in raw-infos (list 'reviews 'nodes) diff-comments)))

(provide 'code-review-bitbucket)
;;; code-review-bitbucket.el ends here
