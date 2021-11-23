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

(require 'code-review-core)
(require 'code-review-db)
(require 'let-alist)
(require 'subr)
(require 'subr-x)

(defgroup code-review-gitlab nil
  "User options for GitLab code reviews."
  :group 'code-review)

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

(defun code-review-gitlab-fix-diff (pr-changes)
  "Get all PR-CHANGES and produce standard git diff."
  (let ((d
         (-map
          (lambda (c)
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
                (format "%s%s%s%s"
                        header1
                        header2
                        header3
                        .diff))))
          pr-changes)))
    (string-join d "")))

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
  (prin1 "AQUI?")
  (let ((d (deferred:new #'identity)))
    (code-review-core-pullreq-infos
     gitlab
     (apply-partially (lambda (d v &rest _)
                        (deferred:callback-post d v))
                      d))
    d))

;; (let-alist (code-review-utils-pr-from-url
;;             "https://gitlab.com/code-review-experiment/default/-/merge_requests/1")
;;   (let ((obj (code-review-gitlab-repo :owner .owner
;;                                       :repo .repo
;;                                       :number .num)))
;;     (code-review-core-pullreq-infos obj (lambda (res &rest v)
;;                                           (prin1 res)))))

(provide 'code-review-gitlab)
;;; code-review-gitlab.el ends here
