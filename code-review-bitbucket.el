;;; code-review-bitbucket.el --- Bitbucket API -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.5
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  Bitbucket API
;;
;;; Code:

(require 'code-review-db)
(require 'code-review-parse-hunk)

(defcustom code-review-bitbucket-host "api.bitbucket.org/2.0"
  "Host used to access Bitbucket API."
  :type 'string
  :group 'code-review)

(defclass code-review-bitbucket-repo (code-review-db-pullreq)
  ((callback            :initform nil)))

(defun code-review-bitbucket--ghub-post (url payload &optional callback)
  "Given URL and PAYLOAD perform a POST.
An optionally provide a CALLBACK."
  (ghub-request "POST" url
                nil
                :forge 'bitbucket
                :auth 'code-review
                :host code-review-bitbucket-host
                :payload payload
                :callback callback))

(defun code-review-bitbucket--ghub-get (url &optional callback)
  "Given URL and PAYLOAD perform a GET.
An optionally provide a CALLBACK."
  (ghub-request "GET" url
                nil
                :forge 'bitbucket
                :auth 'code-review
                :host code-review-bitbucket-host
                :callback callback))

(cl-defmethod code-review-pullreq-diff ((bitbucket code-review-bitbucket-repo) callback)
  "Get PR diff from BITBUCKET, run CALLBACK."
  (let ((diff-url (let-alist (code-review-bitbucket--ghub-get
                              (format "/repositories/%s/%s/pullrequests/%s"
                                      (oref bitbucket owner)
                                      (oref bitbucket repo)
                                      (oref bitbucket number)))
                    (-> .links.diff.href
                        (split-string code-review-bitbucket-host)
                        (-second-item)))))
    (code-review-bitbucket--ghub-get diff-url callback)))

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

(defun code-review-bitbucket--top-level-comments (comments)
  (->> comments
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

(defun code-review-bitbucket--diff-comments (comments)
  (->> comments
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
                                  (databaseId . ,.id)
                                  (path . ,.inline.path)
                                  (createdAt . ,.created_on)))))))))))

(cl-defmethod code-review-pullreq-infos ((bitbucket code-review-bitbucket-repo) callback)
  "Get PR infos from BITBUCKET, run CALLBACK."
  (let* ((res (code-review-bitbucket--ghub-get
               (format "/repositories/%s/%s/pullrequests/%s"
                       (oref bitbucket owner)
                       (oref bitbucket repo)
                       (oref bitbucket number))))
         (comments (->> (code-review-bitbucket--ghub-get
                         (format "/repositories/%s/%s/pullrequests/%s/comments?q=deleted=false&pagelen=100"
                                 (oref bitbucket owner)
                                 (oref bitbucket repo)
                                 (oref bitbucket number)))
                        (-remove
                         (lambda (it)
                           (a-get it 'deleted)))))
         (top-level-comments (code-review-bitbucket--top-level-comments comments))
         (diff-comments (code-review-bitbucket--diff-comments comments)))
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

(defun code-review-bitbucket--inline-arg (comment)
  "Return inline argument used in Bitbucket COMMENT requests."
  (let ((mapping
         (alist-get (oref comment path)
                    code-review-bitbucket-line-diff-mapping
                    nil nil 'equal)))
    (pcase (oref comment line-type)
      ("ADDED"
       `((path . ,(oref c path))
         (to . ,(code-review-parse-hunk-line-pos
                 mapping
                 `((added . t)
                   (line-pos . ,(oref comment position)))))))
      ("REMOVED"
       `((path . ,(oref comment path))
         (from . ,(code-review-parse-hunk-line-pos
                   mapping
                   `((deleted . t)
                     (line-pos . ,(oref comment position)))))))
      ("UNCHANGED"
       `((path . ,(oref comment path))
         (from . ,(a-get
                   (code-review-parse-hunk-line-pos
                    mapping
                    `((deleted . t)
                      (line-pos . ,(oref comment position))))
                   'old-line)))))))

(cl-defmethod code-review-send-review ((review code-review-submit-bitbucket-review) callback)
  "Submit review comments given REVIEW and a CALLBACK."
  (let* ((pr (oref review pr))
         (infos (oref pr raw-infos)))
    ;; 1. send all comments to the PR
    (dolist (c (oref review local-comments))
      (code-review-bitbucket--ghub-post
       (format "/repositories/%s/%s/pullrequests/%s/comments"
               (oref pr owner)
               (oref pr repo)
               (oref pr number))
       `((content (raw . ,(oref c body)))
         (inline . ,(code-review-bitbucket--inline-arg c)))))
    ;; 2. send the review verdict
    (pcase (oref review state)
      ("APPROVE"
       (let ((res (code-review-bitbucket--ghub-post
                   (format "/repositories/%s/%s/pullrequests/%s/approve"
                           (oref pr owner)
                           (oref pr repo)
                           (oref pr number))
                   `((workspace . ,(oref pr owner))))))
         (when (string-equal (a-get res 'type) "error")
           (error (prin1-to-string res)))))
      ("REQUEST_CHANGES"
       (let ((res (code-review-bitbucket--ghub-post
                   (format "/repositories/%s/%s/pullrequests/%s/request-changes"
                           (oref pr owner)
                           (oref pr repo)
                           (oref pr number))
                   `((workspace . ,(oref pr owner))))))
         (when (string-equal (a-get res 'type) "error")
           (error (prin1-to-string res)))))
      ("COMMENT"))
    ;; 3. call callback
    ;; seems like we need to wait a bit for bitbucket's API to update internally :/
    (sit-for 0.5)
    (funcall callback)))

(defclass code-review-submit-bitbucket-replies ()
  ((pr      :initform nil)
   (replies :initform nil
            :type (satisfies
                   (lambda (it)
                     (-all-p #'code-review-submit-reply-p it))))))

(cl-defmethod code-review-send-replies ((replies code-review-submit-bitbucket-replies) callback)
  "Submit replies to review comments inline given REPLIES and a CALLBACK fn."
  (let ((pr (oref replies pr)))
    (dolist (reply (oref replies replies))
      (let ((res (code-review-bitbucket--ghub-post
                  (format "/repositories/%s/%s/pullrequests/%s/comments"
                          (oref pr owner)
                          (oref pr repo)
                          (oref pr number))
                  `((content (raw . ,(oref reply body)))
                    (parent (id . ,(oref reply reply-to-id)))))))
        (when (string-equal (a-get res 'type) "error")
          (error (prin1-to-string res)))))
    (sit-for 0.5)
    (funcall callback)))

;;; fixes

(defvar code-review-bitbucket-line-diff-mapping nil
  "Hold structure to convert Line number position into diff positions.
For internal usage only.")

(defun code-review-bitbucket-pos-line-number->diff-line-number (bitbucket-diff-raw)
  "Get mapping of pos-line to diff-line given BITBUCKET-DIFF-RAW."
  (let* ((bitbucket-diff (-> bitbucket-diff-raw
                             (split-string "diff --git")
                             (cdr)))
         (regex (rx "--- a/"
                    (group-n 1 (+? anything))
                    "+++ b/"
                    (group-n 2 (+? anything))))
         (res
          (-reduce-from
           (lambda (acc it)
             (save-match-data
               (if (string-match regex it)
                   (let* ((path-1 (match-string 1 it))
                          (path-2 (match-string 2 it))
                          (path (if (string-equal path-1 "/dev/null")
                                    path-2
                                  path-1))
                          (hunkdiff (->> it
                                         (s-split (rx "+++ b/" (+ not-newline)))
                                         (-second-item )
                                         (string-trim))))
                     (a-assoc acc (string-trim path) (code-review-parse-hunk-table hunkdiff)))
                 acc)))
           nil bitbucket-diff)))
    (setq code-review-bitbucket-line-diff-mapping res)))

(defun code-review-bitbucket--fix-diff-comments (raw-infos)
  "Fix RAW-INFOS diff comments position to be diff-pos instead of line number."
  (let ((diff-comments
         (-map
          (lambda (it)
            (let-alist it
              (let* ((path (string-trim .path))
                     (mapping (alist-get path
                                         code-review-bitbucket-line-diff-mapping
                                         nil nil 'equal))
                     (diff-pos
                      (cond
                       ((eq :to .position-type)
                        (code-review-parse-hunk-relative-pos mapping `((new . t) (line-pos . ,.position))))
                       ((eq :from .position-type)
                        (code-review-parse-hunk-relative-pos mapping `((old . t) (line-pos . ,.position))))))
                     (cn (a-assoc (-first-item .comments.nodes) 'originalPosition diff-pos)))
                (-> it
                    (a-assoc 'position diff-pos)
                    (a-assoc-in (list 'comments 'nodes) (list cn))))))
          (a-get-in raw-infos (list 'reviews 'nodes)))))
    (a-assoc-in raw-infos (list 'reviews 'nodes) diff-comments)))

(provide 'code-review-bitbucket)
;;; code-review-bitbucket.el ends here
