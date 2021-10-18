;;; code-review-github.el --- Github API functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>

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
;;  This library is an interface with Github REST API and GraphQL API.
;;
;;; Code:

(require 'ghub)
(require 'deferred)
(require 'a)

(defgroup code-review-github nil
  "Interact with GitHub REST and GraphQL APIs."
  :group 'tools)

(defcustom code-review-github-host "api.github.com"
  "Host for the GitHub api if you use the hosted version of GitHub."
  :group 'code-review-github
  :type 'string)

(defconst code-review-github-diffheader '(("Accept" . "application/vnd.github.v3.diff"))
  "Header for requesting diffs from GitHub.")

(defconst code-review-github-token-scopes '(repo)
  "Only repo scope needed to read PRs and submit reviews.")

(defun code-review-github-errback (&rest m)
  "Error callback, displays the error message M."
  (message "Error talking to GitHub: %s" m))

(defun code-review-github-get-diff (pr-alist callback)
  "Get a pull request or its diff.
PR-ALIST is an alist representing a PR,
NEEDS-DIFF t to return a diff, nil to return the pr object
CALLBACK to call back when done."
  (let-alist pr-alist
    (ghub-get (format "/repos/%s/%s/pulls/%s" .owner .repo .num)
              nil
              :unpaginate t
              :headers code-review-github-diffheader
              :auth 'code-review
              :host code-review-github-host
              :callback callback
              :errorback #'code-review-github-errback)))

(defun code-review-github-get-diff-deferred (pr-alist)
  "Get a pull request or its diff.
PR-ALIST is an alist representing a PR,
NEEDS-DIFF t to return a diff nil to return the pr object
return a deferred object"
  (let ((d (deferred:new #'identity)))
    (code-review-github-get-diff
     pr-alist
     (apply-partially
      (lambda (d v &rest _)
        (deferred:callback-post d v))
      d))
    d))

(defun code-review-github-get-pr-info (pr-alist callback)
  "Get PR details from PR-ALIST and dispatch to CALLBACK."
  (let-alist pr-alist
    (let ((query (format "query {
  repository(name: \"%s\", owner: \"%s\") {
    pullRequest(number: %s){
      headRef { target{ oid } }
      baseRefName
      headRefName
      number
      milestone {
        title
        progressPercentage
      }
      labels(first: 3) {
        nodes {
          name
          color
        }
      }
      assignees(first: 15) {
        nodes {
          name
          login
        }
      }
      projectCards(first: 3) {
        nodes {
          project {
            name
          }
        }
      }
      suggestedReviewers {
        reviewer {
          name
        }
      }
      commits(first: 100) {
        totalCount
        nodes {
          commit {
            abbreviatedOid
            message
          }
        }
      }
      title
      state
      bodyText
      comments(first:50) {
        nodes { author { login } bodyText }
      }
      reviews(first: 50) {
        nodes { author { login } bodyText state
          comments(first: 50) {
            nodes {
              bodyText
              originalPosition
              diffHunk
              position
              outdated
              path
              databaseId
            }
          }
        }
      }
    }
  }
}" .repo .owner .num)))
      (ghub-graphql query
                    '()
                    :auth 'code-review
                    :host code-review-github-host
                    :callback callback
                    :errorback #'code-review-github-errback))))

(defun code-review-github-get-pr-info-deferred (pr-alist)
  "Get the code reviews on a PR.
PR-ALIST is an alist representing a PR
returns a deferred object"
  (let ((d (deferred:new #'identity)))
    (code-review-github-get-pr-info
     pr-alist
     (apply-partially (lambda (d v &rest _)
                        (deferred:callback-post d v))
                      d))
    d))

(defun code-review-github-post-review (pr-alist review callback)
  "Submit a code review.
PR-ALIST is an alist representing a PR
REVIEW is the review alist
CALLBACK will be called back when done"
  (let-alist pr-alist
    (ghub-post (format "/repos/%s/%s/pulls/%s/reviews" .owner .repo .num)
               nil
               :auth 'code-review
               :payload (a-assoc review 'event .event)
               :host code-review-github-host
               :errorback #'code-review-github-errback
               :callback callback)))

(defun code-review-github-post-replies (pr-alist replies callback)
  "Submit REPLIES to review comments inline given PR-ALIST and a CALLBACK fn."
  (deferred:$
    (deferred:parallel
      (-map
       (lambda (reply)
         (lambda ()
           (let-alist pr-alist
             (let* ((database-id (a-get reply 'database-id))
                    (body (a-get reply 'body)))
               (ghub-post (format "/repos/%s/%s/pulls/%s/comments/%s/replies"
                                  .owner .repo .num database-id)
                          nil
                          :payload (a-alist 'body body)
                          :headers code-review-github-diffheader
                          :auth 'code-review
                          :host code-review-github-host
                          :callback (lambda (&rest _))
                          :errorback #'code-review-github-errback)))))
       replies))

    (deferred:nextc it
      (lambda (x)
        (funcall callback)))

    (deferred:error it
      (lambda (err)
        (message "Got an error from the Github Reply API %S!" err)))))

(provide 'code-review-github)
;;; code-review-github.el ends here
