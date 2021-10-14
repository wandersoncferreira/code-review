;;; code-review-github.el --- Github API functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/wandersoncferreira/code-review-github
;; Package-Requires: ((emacs "25.1") (a "0.1.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'ghub)
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
              :auth 'code-review-github
              :host code-review-github-host
              :callback callback
              :errorback #'code-review-github-errback)))

(provide 'code-review-github)
;;; code-review-github.el ends here
