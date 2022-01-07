;;; code-review-gitlab.el --- Gitlab API functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.6
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

(require 'dash)
(require 'let-alist)
(require 'code-review-db)
(require 'code-review-parse-hunk)
(require 'code-review-interfaces)
(require 'code-review-utils)

(defgroup code-review-gitlab nil
  "Interact with Gitlab REST and GraphQL APIs."
  :group 'code-review
  :link '(custom-group-link 'code-review-github))

(defcustom code-review-gitlab-host "gitlab.com/api"
  "Host for the Gitlab api if you use the hosted version of Gitlab."
  :group 'code-review-gitlab
  :type 'string)

(defcustom code-review-gitlab-graphql-host "gitlab.com/api"
  "Host for Graphql in Gitlab."
  :group 'code-review-gitlab
  :type 'string)

(defclass code-review-gitlab-repo (code-review-db-pullreq)
  ((callback :initform nil)))

 (defvar code-review-gitlab-line-diff-mapping nil
  "Hold structure to convert Line number position into diff positions.
For internal usage only.")

;;; helpers

(defun code-review-gitlab--project-id (pr)
  "Return the project ID for a PR."
  (let ((owner (replace-regexp-in-string "/" "%2F" (oref pr owner))))
    (format "%s%%2F%s" owner (oref pr repo))))

(defun code-review-gitlab-errback (&rest m)
  "Error callback, displays the error message M."
  (let-alist m
    (code-review-utils--log
     "code-review-gitlab-errback"
     (prin1-to-string m))
    (error "Unknown error talking to Gitlab: %s" m)))

(defun code-review-gitlab--graphql (graphql variables callback)
  "Make GRAPHQL call to GITLAB.
Optionally using VARIABLES.  Provide HOST and CALLBACK fn."
  (glab-request "POST" "/graphql" nil :payload (json-encode
                                                `(("query" . ,graphql)
                                                  ,@(and variables `(("variables" ,@variables)))))
                :auth 'code-review
                :host code-review-gitlab-graphql-host
                :callback callback
                :errorback #'code-review-gitlab-errback))

;;; Functions to standardize Gitlab returned datastructure to the ones used by
;;; Github, adopted as standard in the project conception :/.

(defun code-review-gitlab-fix-diff (pr-changes)
  "Get all PR-CHANGES and produce standard git diff.
Unfortunately, Gitlab's API returns the elements of the diff in
an object then we need to build the diff string ourselves here."
  (string-remove-suffix
   "\n"
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
    pr-changes)))

(defun code-review-gitlab--review-comments (all-comments)
  "Return only review comments from ALL-COMMENTS."
  (--sort
   (> (time-to-seconds (date-to-time (a-get it 'createdAt)))
      (time-to-seconds (date-to-time (a-get other 'createdAt))))
   (-filter
    (lambda (c)
      (and (not (code-review-gitlab--regular-comment? c))
           (not (a-get c 'system))))
    all-comments)))

(defun code-review-gitlab--overview-comments (all-comments)
  "Return only overview comments from ALL-COMMENTS."
  (nreverse
   (-filter
    (lambda (c)
      (code-review-gitlab--regular-comment? c))
    all-comments)))

(defun code-review-gitlab--review-comment->code-review-comment (comment)
  "Transform a Gitlab review COMMENT into `code-review-comment' structure."
  (let-alist comment
    (let* ((path .position.oldPath)
           (mapping  (alist-get .position.oldPath
                                code-review-gitlab-line-diff-mapping
                                nil nil 'equal))
           (line-obj (if .position.oldLine
                         `((old . t)
                           (line-pos . ,.position.oldLine))
                       `((new . t)
                         (line-pos . ,.position.newLine))))
           (discussion-id (-> .discussion.id
                              (split-string "DiffDiscussion/")
                              (-second-item)))
           (diff-pos (code-review-parse-hunk-relative-pos mapping line-obj)))
      `((author (login . ,.author.login))
        (state . ,"")
        (bodyHTML .,"")
        (createdAt . ,.createdAt)
        (updatedAt . ,.updatedAt)
        (comments (nodes ((bodyHTML . ,.bodyHTML)
                          (path . ,.position.oldPath)
                          (position . ,diff-pos)
                          (databaseId . ,discussion-id)
                          (createdAt . ,.createdAt)
                          (updatedAt . ,.updatedAt))))))))

(defun code-review-gitlab-fix-review-comments (all-comments)
  "Get `code-review-comment' structure out of ALL-COMMENTS to all review comments."
  (let* ((review-comments (code-review-gitlab--review-comments all-comments))
         (grouped-comments (-group-by
                            (lambda (c)
                              (let ((line (or (a-get-in c (list 'position 'oldLine))
                                              (a-get-in c (list 'position 'newLine))))
                                    (path (a-get-in c (list 'position 'oldPath))))
                                ;; we assume that every review comment requires
                                ;; a positional line number to be possible to
                                ;; place it in the diff. However, Gitlab's API
                                ;; does not provide a good differentiation
                                ;; between an Overview comment and a Diff
                                ;; comment so the heuristic used in
                                ;; `code-review-gitlab--review-comments' might
                                ;; be incomplete.
                                (if (not (numberp line))
                                    (error "Review Comment
                                    without position line number
                                    found! Possibly a bug in
                                    heuristic to identify Review
                                    Comments")
                                  (concat path ":" (number-to-string line)))))
                            review-comments)))
    (-reduce-from
     (lambda (acc k)
       (let* ((comments (alist-get k grouped-comments nil nil 'equal)))
         (if (> (length comments) 1)
             (append acc
                     (-map
                      (lambda (c)
                        (code-review-gitlab--review-comment->code-review-comment c))
                      (nreverse comments)))
           (cons (code-review-gitlab--review-comment->code-review-comment
                  (-first-item comments))
                 acc))))
     nil
     (a-keys grouped-comments))))

(defun code-review-gitlab--regular-comment? (comment)
  "Predicate to identify regular (overview) COMMENT from review comment."
  (let-alist comment
    (and (not .system)
         (or (not .resolvable)
             (not .position)))))

(defun code-review-gitlab-fix-infos (gitlab-infos)
  "Make GITLAB-INFOS structure compatible with GITHUB."
  (let ((all-comments (a-get-in gitlab-infos (list 'comments 'nodes))))
    (let-alist gitlab-infos
      (-> gitlab-infos
          (a-assoc 'commits
                   (a-alist 'totalCount .commitCount
                            'nodes (-map
                                    (lambda (c)
                                      (a-alist 'commit c))
                                    .commitsWithoutMergeCommits.nodes)))
          (a-assoc 'comments
                   (a-alist 'nodes (code-review-gitlab--overview-comments all-comments)))
          (a-assoc 'reviews
                   (a-alist 'nodes (code-review-gitlab-fix-review-comments all-comments)))))))

(defun code-review-gitlab-fix-payload (payload comment)
  "Adjust the PAYLOAD based on the COMMENT.
The payload is used to send a MR review to Gitlab."
  (let* ((mapping (alist-get (oref comment path)
                             code-review-gitlab-line-diff-mapping
                             nil nil 'equal))
         (line-type (oref comment line-type))
         (pos (oref comment position)))
    (pcase line-type
      ("ADDED"
       (a-assoc-in payload (list 'position 'new_line)
                   (code-review-parse-hunk-line-pos mapping `((added . t) (line-pos . ,pos)))))
      ("REMOVED"
       (a-assoc-in payload (list 'position 'old_line)
                   (code-review-parse-hunk-line-pos mapping `((deleted . t) (line-pos . ,pos)))))
      ("UNCHANGED"
       (let ((line-pos-res
              (code-review-parse-hunk-line-pos
               mapping
               `((normal . t) (line-pos . ,pos)))))
         (-> payload
             (a-assoc-in (list 'position 'new_line) (a-get line-pos-res 'new-line))
             (a-assoc-in (list 'position 'old_line) (a-get line-pos-res 'old-line))))))))

;;; classes

(defclass code-review-submit-gitlab-replies ()
  ((pr      :initform nil)
   (replies :initform nil
            :type (satisfies
                   (lambda (it)
                     (-all-p #'code-review-submit-reply-p it))))))

(defclass code-review-submit-gitlab-review ()
  ((state :initform nil)
   (pr :initform nil)
   (local-comments :initform nil
                   :type (satisfies
                          (lambda (it)
                            (-all-p #'code-review-submit-local-coment-p it))))
   (feedback :initform nil)))

;;; reify

(cl-defmethod code-review-pullreq-diff ((gitlab code-review-gitlab-repo) callback)
  "Get PR diff from GITLAB, run CALLBACK after answer."
  (glab-get (format "/v4/projects/%s/merge_requests/%s/changes?access_raw_diffs=true"
                    (code-review-gitlab--project-id gitlab)
                    (oref gitlab number))
            nil
            :unpaginate t
            :host code-review-gitlab-host
            :auth 'code-review
            :callback callback
            :errorback #'code-review-gitlab-errback))

(cl-defmethod code-review-diff-deferred ((gitlab code-review-gitlab-repo))
  "Get DIFF from GITLAB."
  (let ((d (deferred:new #'identity)))
    (code-review-pullreq-diff
     gitlab
     (apply-partially
      (lambda (d v &rest _)
        (deferred:callback-post d v))
      d))
    d))

(cl-defmethod code-review-pullreq-infos ((gitlab code-review-gitlab-repo) fallback? callback)
  "Get PR details from GITLAB, choose minimal query on FALLBACK? and dispatch to CALLBACK."
  (let* ((owner (oref gitlab owner))
         (repo (oref gitlab repo))
         (repo-clean (replace-regexp-in-string "%2F" "/" repo))
         (number (oref gitlab number))
         (query
          (format "query{
repository:project(fullPath: \"%s\") {
    pullRequest:mergeRequest(iid: \"%s\") {
      id
      author {
        login:username
        url:webUrl
      }
      comments:notes(first: 50){
        nodes {
          databaseId:id
          discussion {
            id
          }
          bodyHTML:bodyHtml
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
      bodyHTML:descriptionHtml
    }
  }
}
" (format "%s/%s" owner repo-clean) number)))
    (code-review-gitlab--graphql
     query
     nil
     callback)))

(cl-defmethod code-review-infos-deferred ((gitlab code-review-gitlab-repo) &optional fallback?)
  "Get PR infos from GITLAB.
Optionally sets FALLBACK? to get minimal query."
  (let ((d (deferred:new #'identity)))
    (code-review-pullreq-infos
     gitlab
     fallback?
     (apply-partially
      (lambda (d v &rest _)
        (deferred:callback-post d v))
      d))
    d))

(defun code-review-gitlab-pos-line-number->diff-line-number (gitlab-diff)
  "Get mapping of pos-line to diff-line given GITLAB-DIFF."
  (let* ((res
          (-reduce-from
           (lambda (acc it)
             (let ((str (a-get it 'diff)))
               (a-assoc acc (or (a-get it 'old_path)
                                (a-get it 'new_path))
                        (code-review-parse-hunk-table str))))
           nil
           gitlab-diff)))
    (setq code-review-gitlab-line-diff-mapping res)))

(cl-defmethod code-review-send-replies ((replies code-review-submit-gitlab-replies) callback)
  "Submit replies to review comments inline given REPLIES and a CALLBACK fn."
  (let ((pr (oref replies pr)))
    (deferred:$
      (deferred:parallel
        (-map
         (lambda (reply)
           (lambda ()
             (glab-post (format "/v4/projects/%s/merge_requests/%s/discussions/%s/notes"
                                (code-review-gitlab--project-id pr)
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
          (error "Got an error from the Gitlab Reply API %S!" err))))))

(defun code-review-gitlab--user ()
  "Get the user in the authinfo file."
  (-> (auth-source-search :host code-review-gitlab-host)
      (car)
      (plist-get :user)
      (split-string "\\^")
      (car)))

(cl-defmethod code-review-send-review ((review code-review-submit-gitlab-review) callback)
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
        (glab-post (format "/v4/projects/%s/merge_requests/%s/discussions"
                           (code-review-gitlab--project-id pr)
                           (oref pr number))
                   nil
                   :auth 'code-review
                   :host code-review-gitlab-host
                   :payload (code-review-gitlab-fix-payload payload c)
                   :callback (lambda (&rest _)
                               (message "Review Comments successfully!")))))
    ;; 2. send the review verdict
    (pcase (oref review state)
      ("APPROVE"
       (glab-post (format "/v4/projects/%s/merge_requests/%s/approve"
                          (code-review-gitlab--project-id pr)
                          (oref pr number))
                  nil
                  :auth 'code-review
                  :host code-review-gitlab-host
                  :payload `((sha . ,(a-get-in infos (list 'diffRefs 'headSha))))
                  :username (code-review-gitlab--user)))
      ("REQUEST_CHANGES"
       (error "Not supported in Gitlab"))
      ("COMMENT"
       (glab-post (format "/v4/projects/%s/merge_requests/%s/discussions"
                          (code-review-gitlab--project-id pr)
                          (oref pr number))
                  nil
                  :auth 'code-review
                  :host code-review-gitlab-host
                  :payload `((body . ,(oref review feedback))))
       (message "Review Comment successfully sent!")))

    ;; 3. call callback
    ;; seems like we need to wait a bit for gitlab's API to update the new reply record
    (sit-for 0.5)
    (funcall callback)))

;;; Core methods not implemented yet.

(defun code-review-gitlab-not-supported-message ()
  "Default warning message."
  (message "Not supported in Gitlab yet.")
  nil)

(cl-defmethod code-review-get-assignable-users ((_gitlab code-review-gitlab-repo))
  "Get a list of assignable users for current PR at GITLAB."
(code-review-gitlab-not-supported-message))

(cl-defmethod code-review-get-labels ((gitlab code-review-gitlab-repo))
  "Get labels for your pr at GITLAB."
  (let ((res (glab-get (format "/v4/projects/%s/labels"
                               (code-review-gitlab--project-id gitlab))
                       nil
                       :unpaginate t
                       :host code-review-gitlab-host
                       :auth 'code-review
                       :noerror 'return)))
    (if (a-get res 'error)
        (error (prin1-to-string res))
      (-map
       (lambda (it)
         (a-get it 'name))
       res))))

(cl-defmethod code-review-send-labels ((gitlab code-review-gitlab-repo) callback)
  "Set labels for your pr at GITLAB and call CALLBACK."
  (let* ((labels (-map (lambda (it) (a-get it 'name)) (oref gitlab labels)))
         (labels-str (string-join labels ",")))
    (glab-put (format "/v4/projects/%s/merge_requests/%s"
                      (code-review-gitlab--project-id gitlab)
                      (oref gitlab number))
              nil
              :auth 'code-review
              :host code-review-gitlab-host
              :payload `((labels .,labels-str))
              :callback (lambda (&rest _) (funcall callback)))))

(cl-defmethod code-review-get-assignees ((_gitlab code-review-gitlab-repo))
  "Get assignees for your pr at GITLAB."
  (code-review-gitlab-not-supported-message))

(cl-defmethod code-review-send-assignee ((_gitlab code-review-gitlab-repo) _callback)
  "Set yourself as assignee in GITLAB and call CALLBACK."
  (code-review-gitlab-not-supported-message))

(cl-defmethod code-review-get-milestones ((_gitlab code-review-gitlab-repo))
  "Get milestones for your pr at GITLAB."
  (code-review-gitlab-not-supported-message))

(cl-defmethod code-review-send-milestone ((_gitlab code-review-gitlab-repo) _callback)
  "Set milestone for your pr in GITLAB and call CALLBACK."
  (code-review-gitlab-not-supported-message))

(cl-defmethod code-review-send-title ((gitlab code-review-gitlab-repo) callback)
  "Set title for your pr in GITLAB and call CALLBACK."
  (glab-put (format "/v4/projects/%s/merge_requests/%s"
                    (code-review-gitlab--project-id gitlab)
                    (oref gitlab number))
            nil
            :auth 'code-review
            :host code-review-gitlab-host
            :payload `((title .,(oref gitlab title)))
            :callback (lambda (&rest _) (funcall callback))))

(cl-defmethod code-review-send-description ((_gitlab code-review-gitlab-repo) _callback)
  "Set description for your pr in GITLAB and call CALLBACK."
  (code-review-gitlab-not-supported-message))

(cl-defmethod code-review-merge ((_gitlab code-review-gitlab-repo) _strategy)
  "Merge a pr in GITLAB using STRATEGY."
  (code-review-gitlab-not-supported-message))

(cl-defmethod code-review-send-reaction ((_gitlab code-review-gitlab-repo))
  "Set reaction for your pr in GITLAB."
  (code-review-gitlab-not-supported-message))

(cl-defmethod code-review-binary-file-url ((gitlab code-review-gitlab-repo) filename &optional blob?)
  "Make the GITLAB url for the FILENAME.
Return the blob URL if BLOB? is provided."
  (let ((sha (a-get-in (oref gitlab raw-infos) (list 'diffRefs 'headSha))))
    (if blob?
        (format "https://gitlab.com/%s/%s/-/blob/%s/%s"
                (oref gitlab owner)
                (oref gitlab repo)
                sha
                filename)
      (format "https://%s/v4/projects/%s/repository/files/%s/raw?ref=%s"
              code-review-gitlab-host
              (code-review-gitlab--project-id gitlab)
              filename
              sha))))

(cl-defmethod code-review-binary-file ((gitlab code-review-gitlab-repo) filename)
  "Get FILENAME from GITLAB."
  (let* ((pwd (auth-source-pick-first-password :host code-review-gitlab-host))
         (headers (format "--header 'PRIVATE-TOKEN: %s'" pwd))
         (url (code-review-binary-file-url gitlab filename)))
    (code-review-utils--fetch-binary-data url filename headers)))

(cl-defmethod code-review-new-issue-comment ((gitlab code-review-gitlab-repo) comment-msg callback)
  "Create a new comment issue for GITLAB sending the COMMENT-MSG and call CALLBACK."
  (glab-post (format "/v4/projects/%s/merge_requests/%s/notes"
                     (code-review-gitlab--project-id gitlab)
                     (oref gitlab number))
             nil
             :auth 'code-review
             :host code-review-gitlab-host
             :payload (a-alist 'body comment-msg)
             :callback callback
             :errorback #'code-review-gitlab-errback))

(cl-defmethod code-review-new-code-comment ((gitlab code-review-gitlab-repo) local-comment callback)
  "Creare a new code comment in GITLAB from a LOCAL-COMMENT and call CALLBACK."
  (let* ((infos (oref gitlab raw-infos))
         (payload (a-alist 'body (oref local-comment msg)
                           'position (a-alist 'position_type "text"
                                              'base_sha (a-get-in infos (list 'diffRefs 'baseSha))
                                              'head_sha (a-get-in infos (list 'diffRefs 'headSha))
                                              'start_sha (a-get-in infos (list 'diffRefs 'startSha))
                                              'new_path (oref local-comment path)
                                              'old_path (oref local-comment path)))))
    (glab-post (format "/v4/projects/%s/merge_requests/%s/discussions"
                       (code-review-gitlab--project-id gitlab)
                       (oref gitlab number))
               nil
               :auth 'code-review
               :host code-review-gitlab-host
               :payload (code-review-gitlab-fix-payload payload local-comment)
               :callback (lambda (&rest _)
                           (message "Review Comments successfully!")
                           (sit-for 0.5)
                           (funcall callback))
               :errorback #'code-review-gitlab-errback)))

(provide 'code-review-gitlab)
;;; code-review-gitlab.el ends here
