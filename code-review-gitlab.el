;;; code-review-gitlab.el --- Gitlab API functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.1
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
;;  This library is an interface with Gitlab API.
;;
;;; Code:

(require 'code-review-db)
(require 'code-review-core)
(require 'code-review-utils)
(require 'let-alist)

(defcustom code-review-gitlab-host "gitlab.com/api/v4"
  "Host for the Gitlab api if you use the hosted version of Gitlab."
  :group 'code-review-gitlab
  :type 'string)

(defcustom code-review-gitlab-graphql-host "gitlab.com/api"
  "Host for Graphql in Gitlab."
  :group 'code-review-gitlab
  :type 'string)

(defclass code-review-gitlab-repo (code-review-db-pullreq)
  ((callback :initform nil)))

(defvar code-review-log-file)

(defvar code-review-gitlab-line-diff-mapping nil
  "Hold structure to convert Line number position into diff positions.
For internal usage only.")

(defun code-review-gitlab-errback (&rest m)
  "Error callback, displays the error message M."
  (let-alist m
    (code-review-utils--log
     "code-review-gitlab-errback"
     (prin1-to-string m))
    (message "Unknown error talking to Gitlab: %s" m)))

(defun code-review-gitlab-fix-diff (pr-changes)
  "Get all PR-CHANGES and produce standard git diff."
  (-reduce-from
   (lambda (acc c)
     (let-alist c
       (let ((header1 (format "diff --git %s %s\n" .new_path .old_path))
             (header2 (cond
                       (.deleted_file
                        (format "deleted file mode %s\n" .a_mode))
                       (.new_file
                        (format "new file mode %s\nindex 0000000000000000000000000000000000000000..1111\n" .b_mode))
                       (.renamed_file)
                       (t
                        (format "index 1111..2222 %s\n" .a_mode))))
             (header3 (cond
                       (.deleted_file
                        (format "--- %s\n+++ /dev/null\n" .old_path))
                       (.new_file
                        (format "--- /dev/null\n+++ %s\n" .new_path))
                       (.renamed_file)
                       (t
                        (format "--- %s\n+++ %s\n"
                                .old_path
                                .new_path)))))
         (format "%s%s%s%s%s"
                 acc
                 header1
                 header2
                 header3
                 .diff))))
   ""
   pr-changes))

(cl-defmethod code-review-core-pullreq-diff ((gitlab code-review-gitlab-repo) callback)
  "Get PR diff from GITLAB, run CALLBACK after answer."
  (glab-get (format "/projects/%s/merge_requests/%s/changes"
                    (format "%s%%2F%s" (oref gitlab owner) (oref gitlab repo))
                    (oref gitlab number))
            nil
            :unpaginate t
            :auth 'code-review
            :callback callback))

(cl-defmethod code-review-core-diff-deferred ((gitlab code-review-gitlab-repo))
  "Get DIFF from GITLAB."
  (let ((d (deferred:new #'identity)))
    (code-review-core-pullreq-diff
     gitlab
     (apply-partially
      (lambda (d v &rest _)
        (deferred:callback-post d v))
      d))
    d))

(defun code-review-gitlab--graphql (graphql callback)
  "Make GRAPHQL call to GITLAB.
Optionally using VARIABLES. Provide HOST and CALLBACK fn."
  (glab-request "POST" "/graphql" nil
                :payload (json-encode `(("query" . ,graphql)))
                :auth 'code-review
                :host code-review-gitlab-graphql-host
                :callback callback))

(cl-defmethod code-review-core-pullreq-infos ((gitlab code-review-gitlab-repo) callback)
  "Get PR details from GITLAB and dispatch to CALLBACK."
  (let* ((owner (oref gitlab owner))
         (repo (oref gitlab repo))
         (number (oref gitlab number))
         (query (format "query{
repository:project(fullPath: \"%s\") {
    pullRequest:mergeRequest(iid: \"%s\") {
      id
      comments:notes(first: 50){
        nodes {
          databaseId:id
          discussion {
            id
          }
          bodyText: body
          author {
            login:username
          }
          createdAt
          updatedAt
          system
          resolvable
          position {
            height
            newLine
            newPath
            oldLine
            oldPath
            width
            x
            y
          }
        }
      }
      diffRefs {
        baseSha
        headSha
        startSha
      }
      headRefName:sourceBranch
      baseRefName:targetBranch
      commitCount
      commitsWithoutMergeCommits(first: 100) {
        nodes {
          abbreviatedOid:shortId
          message
        }
      }
      number: iid
      isDraft: draft
      databaseId: iid
      createdAt
      updatedAt
      milestone {
        title
      }
      labels(first: 10) {
        nodes{
          color
          name: title
        }
      }
      assignees(first: 15) {
        nodes{
          name
          login: username
        }
      }
      title
      state
      bodyText: description
    }
  }
}" (format "%s/%s" owner repo) number)))
    (code-review-gitlab--graphql query callback)))

(cl-defmethod code-review-core-infos-deferred ((gitlab code-review-gitlab-repo))
  "Get PR infos from GITLAB."
  (let ((d (deferred:new #'identity)))
    (code-review-core-pullreq-infos
     gitlab
     (apply-partially
      (lambda (d v &rest _)
        (deferred:callback-post d v))
      d))
    d))

(defun code-review-gitlab-fix-review-comments (raw-comments)
  "Format RAW-COMMENTS to be compatible with established shape in the package."
  (let* ((review-comments (-filter
                           (lambda (c)
                             (and (not (a-get c 'system))
                                  (a-get c 'resolvable)))
                           raw-comments))
         (grouped-comments (-group-by
                            (lambda (c)
                              (let ((line (or (a-get-in c (list 'position 'oldLine))
                                              (a-get-in c (list 'position 'newLine))))
                                    (path (a-get-in c (list 'position 'oldPath))))
                                (concat path ":" (number-to-string line))))
                            review-comments))
         (comment->code-review-comment
          (lambda (c)
            (let-alist c
              (let* ((mapping  (alist-get .position.oldPath code-review-gitlab-line-diff-mapping nil nil 'equal))
                     (diff-pos
                      ;; NOTE: not sure if this should not be a little different in the future
                      ;; e.g. verify if the comment was done in Added/Removed/Unchanged line
                      ;; and handling accordingly.
                      (+ 1 (- (or .position.oldLine
                                  .position.newLine)
                              (or (a-get-in mapping (list 'old 'beg))
                                  (a-get-in mapping (list 'new 'beg)))))))
                `((author (login . ,.author.login))
                  (state . ,"")
                  (bodyText .,"")
                  (createdAt . ,.createdAt)
                  (updatedAt . ,.updatedAt)
                  (comments (nodes ((bodyText . ,.bodyText)
                                    (path . ,.position.oldPath)
                                    (position . ,diff-pos)
                                    (databaseId . ,(-second-item (split-string .discussion.id "DiffDiscussion/")))
                                    (createdAt . ,.createdAt)
                                    (updatedAt . ,.updatedAt))))))))))
    (-reduce-from
     (lambda (acc k)
       (let* ((comments (alist-get k grouped-comments nil nil 'equal)))
         (if (> (length comments) 1)
             (append acc (-map
                          (lambda (c)
                            (funcall comment->code-review-comment c))
                          (nreverse comments)))
           (cons (funcall comment->code-review-comment (-first-item comments)) acc))))
     nil
     (a-keys grouped-comments))))

(defun code-review-gitlab-pos-line-number->diff-line-number (gitlab-diff)
  "Get mapping of pos-line to diff-line given GITLAB-DIFF."
  (let ((if-zero-null (lambda (n)
                        (let ((nn (string-to-number n)))
                          (when (> nn 0)
                            nn))))
        (regex
         (rx "@@ -"
             (group-n 1 (one-or-more digit))
             ","
             (group-n 2 (one-or-more digit))
             " +"
             (group-n 3 (one-or-more digit))
             ","
             (group-n 4 (one-or-more digit)))))
    (setq code-review-gitlab-line-diff-mapping
          (-reduce-from
           (lambda (acc it)
             (let ((str (a-get it 'diff)))
               (save-match-data
                 (and (string-match regex str)
                      (a-assoc acc (a-get it 'old_path)
                               (a-alist 'old (a-alist 'beg (funcall if-zero-null (match-string 1 str))
                                                      'end (funcall if-zero-null (match-string 2 str))
                                                      'path (a-get it 'old_path))
                                        'new (a-alist 'beg (funcall if-zero-null (match-string 3 str))
                                                      'end (funcall if-zero-null (match-string 4 str))
                                                      'path (a-get it 'new_path))))))))
           nil
           gitlab-diff))))


(defun code-review-gitlab-fix-infos (gitlab-infos)
  "Make GITLAB-INFOS structure compatible with GITHUB."
  (let ((comment-nodes (a-get-in gitlab-infos (list 'comments 'nodes))))
    (-> gitlab-infos
        (a-assoc 'commits
                 (a-alist 'totalCount (a-get gitlab-infos 'commitCount)
                          'nodes (-map
                                  (lambda (c)
                                    (a-alist 'commit c))
                                  (a-get-in gitlab-infos (list 'commitsWithoutMergeCommits 'nodes)))))
        (a-assoc 'comments
                 (a-alist 'nodes
                          (-filter
                           (lambda (c)
                             (and (not (a-get c 'system))
                                  (not (a-get c 'resolvable))))
                           comment-nodes)))
        (a-assoc 'reviews
                 (a-alist 'nodes (code-review-gitlab-fix-review-comments comment-nodes))))))

(defclass code-review-submit-gitlab-replies ()
  ((pr      :initform nil)
   (replies :initform nil
            :type (satisfies
                   (lambda (it)
                     (-all-p #'code-review-submit-reply-p it))))))

(cl-defmethod code-review-core-send-replies ((replies code-review-submit-gitlab-replies) callback)
  "Submit replies to review comments inline given REPLIES and a CALLBACK fn."
  (let ((pr (oref replies pr)))
    (deferred:$
      (deferred:parallel
        (-map
         (lambda (reply)
           (lambda ()
             (glab-post (format "/projects/%s/merge_requests/%s/discussions/%s/notes"
                                (format "%s%%2F%s" (oref pr owner) (oref pr repo))
                                (oref pr number)
                                (oref reply reply-to-id))
                        nil
                        :payload (a-alist 'body (oref reply body))
                        :auth 'code-review
                        :host code-review-gitlab-host
                        :errorback #'code-review-gitlab-errback
                        :callback (lambda (&rest _)))))
         (oref replies replies)))

      (deferred:wait 500)

      (deferred:nextc it
        (lambda (_x)
          (funcall callback)))

      (deferred:error it
        (lambda (err)
          (message "Got an error from the Gitlab Reply API %S!" err))))))

(defclass code-review-submit-gitlab-review ()
  ((state :initform nil)
   (pr :initform nil)
   (local-comments :initform nil
                   :type (satisfies
                          (lambda (it)
                            (-all-p #'code-review-submit-local-coment-p it))))
   (feedback :initform nil)))

(defun code-review-gitlab-fix-payload (payload comment)
  "Adjust the PAYLOAD based on the COMMENT."
  (let* ((mapping (alist-get (oref comment path)
                             code-review-gitlab-line-diff-mapping
                             nil nil 'equal))
         (line-type (oref comment line-type))
         (pos (oref comment position)))
    (pcase line-type
      ("ADDED"
       (a-assoc-in payload (list 'position 'new_line)
                   (+ (- pos (a-get-in mapping (list 'new 'end))) 1)))
      ("REMOVED"
       (a-assoc-in payload (list 'position 'old_line)
                   (- (+ pos (a-get-in mapping (list 'old 'beg))) 1)))
      ("UNCHANGED"
       (-> payload
           (a-assoc-in (list 'position 'new_line)
                       (+ (- pos (a-get-in mapping (list 'new 'end))) 1))
           (a-assoc-in (list 'position 'old_line)
                       (- (+ pos (a-get-in mapping (list 'old 'beg))) 1)))))))

(cl-defmethod code-review-core-send-review ((review code-review-submit-gitlab-review) callback)
  "Submit review comments given REVIEW and a CALLBACK fn."
  (let* ((pr (oref review pr))
         (infos (oref pr raw-infos)))
    ;; 1. send all comments to the MR
    (dolist (c (oref review local-comments))
      (let* ((payload (a-alist 'body (oref c body)
                               'position (a-alist 'position_type "text"
                                                  'base_sha (a-get-in infos (list 'diffRefs 'baseSha))
                                                  'head_sha (a-get-in infos (list 'diffRefs 'headSha))
                                                  'start_sha (a-get-in infos (list 'diffRefs 'startSha))
                                                  'new_path (oref c path)
                                                  'old_path (oref c path)))))
        (glab-post (format "/projects/%s/merge_requests/%s/discussions"
                           (format "%s%%2F%s" (oref pr owner) (oref pr repo))
                           (oref pr number))
                   nil
                   :auth 'code-review
                   :payload (code-review-gitlab-fix-payload payload c)
                   :callback (lambda (&rest _)
                               (message "Review Comments successfully!")))))
    ;; 2. send the review verdict
    (pcase (oref review state)
      ("APPROVE"
       (glab-post (format "/projects/%s/merge_requests/%s/approve"
                          (format "%s%%2F%s" (oref pr owner) (oref pr repo))
                          (oref pr number))
                  nil
                  :auth 'code-review
                  :callback (lambda (&rest _) (message "Approved"))))
      ("REQUEST_CHANGES"
       (message "Not supported in Gitlab"))
      ("COMMENT"))

    ;; 3. call callback
    ;; seems like we need to wait a bit for gitlab's API to update the new reply record
    (sit-for 0.5)
    (funcall callback)))

(provide 'code-review-gitlab)
;;; code-review-gitlab.el ends here
