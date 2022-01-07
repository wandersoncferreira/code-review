;;; code-review.el --- Perform code review from Github, Gitlab, and Bitbucket Cloud -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 14, 2021
;; Version: 0.0.6
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review
;; Package-Requires: ((emacs "25.1") (closql "1.2.0") (magit "3.0.0") (a "1.0.0") (ghub "3.5.1") (uuidgen "1.2") (deferred "0.5.1") (markdown-mode "2.4") (forge "0.3.0") (emojify "1.2"))

;; This file is not part of GNU Emacs

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
;;
;;; Commentary:
;;
;; Review Pull Request in Emacs using a modern interface based on Magit Section
;; and Transient.  Currently supports Github, Gitlab, and Bitbucket Cloud.
;;

;;; Code:

(require 'closql)
(require 'magit-section)
(require 'code-review-github)
(require 'code-review-gitlab)
(require 'code-review-section)
(require 'code-review-comment)
(require 'code-review-utils)
(require 'code-review-db)
(require 'code-review-interfaces)
(require 'code-review-faces)
(require 'code-review-actions)

(defgroup code-review nil
  "Code Review tool for VC forges."
  :group 'tools
  :link '(custom-group-link 'code-review-github)
  :link '(custom-group-link 'code-review-gitlab))

(defcustom code-review-headers-hook
  '(code-review-section-insert-header-title
    code-review-section-insert-author
    code-review-section-insert-title
    code-review-section-insert-state
    code-review-section-insert-ref
    code-review-section-insert-milestone
    code-review-section-insert-labels
    code-review-section-insert-project
    code-review-section-insert-is-draft
    code-review-section-insert-assignee
    code-review-section-insert-suggested-reviewers
    code-review-section-insert-reviewers)
  "Hook run to insert headers into the code review buffer."
  :group 'code-review
  :type 'hook)

(defcustom code-review-sections-hook
  '(code-review-section-insert-headers
    code-review-section-insert-commits
    code-review-section-insert-pr-description
    code-review-section-insert-feedback-heading
    code-review-section-insert-top-level-comments)
  "Hook run to insert sections into a code review buffer."
  :group 'code-review
  :type 'hook)

(defcustom code-review-sections-commit-hook
  '(code-review-section-insert-headers)
  "Hook run to insert sections into a code review commit buffer."
  :group 'code-review
  :type 'hook)

(defun code-review-auth-source-debug ()
  "Do not warn on auth source search because it messes with progress reporter."
  (setq-local auth-source-debug (lambda (&rest _))))

;;; Entrypoint

;;;###autoload
(defun code-review-forge-pr-at-point ()
  "Review the forge pull request at point.
OUTDATED."
  (interactive)
  (let ((code-review-section-full-refresh? t)
        (pr-alist (code-review-utils--alist-forge-at-point)))
    (code-review-auth-source-debug)
    (code-review-utils-build-obj pr-alist)
    (code-review--build-buffer code-review-buffer-name)))

;;;###autoload
(defun code-review-start (url)
  "Start review given PR URL."
  (interactive "sURL to review: ")
  (let ((code-review-section-full-refresh? t))
    (code-review-auth-source-debug)
    (code-review-utils-build-obj-from-url url)
    (code-review--build-buffer
     code-review-buffer-name)))

;;; Commit buffer

(define-minor-mode code-review-commit-minor-mode
  "Code Review Commit."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "b") 'code-review-commit-buffer-back)
            map))

;;; Transient

(transient-define-prefix code-review-transient-api ()
  "Code Review."
  [["Review"
    ("a" "Approve" code-review-submit-approve)
    ("r" "Request Changes" code-review-submit-request-changes)
    ("c" "Comment" code-review-submit-comments)
    ("C-c C-s" "Save Unfinished Review" code-review-save-unfinished-review)
    ("C-c C-r" "Open Unfinished Review" code-review-open-unfinished-review)]
   ["Merge"
    ("m m" "Merge" code-review-merge-merge)
    ("m r" "Merge Rebase" code-review-merge-rebase)
    ("m s" "Merge Squash" code-review-merge-squash)]]
  ["Fast track"
   ("l" "LGTM - Approved" code-review-submit-lgtm)
   ("p" "Submit Replies" code-review-submit-only-replies)
   ("s c" "Single Comment, immediately sent" code-review-submit-single-top-level-comment)
   ("s C" "Single Diff Comment, immediately sent" code-review-submit-single-diff-comment-at-point)]
  ["Setters"
   ("s f" "Feedback" code-review-set-feedback)
   ("s r" "Reviewers" code-review-request-reviews)
   ("s y" "Yourself as Assignee" code-review-set-yourself-assignee)
   ("s a" "Assignee" code-review-set-assignee)
   ("s m" "Milestone" code-review-set-milestone)
   ("s l" "Labels" code-review-set-label)
   ("s t" "Title" code-review-set-title)
   ("s d" "Description" code-review-set-description)]
  ["Buffer"
   ("G" "Full reload" code-review-reload)
   ("q" "Quit" transient-quit-one)])

(defvar code-review-mode-map
  (let ((map (copy-keymap magit-section-mode-map)))
    (suppress-keymap map t)
    (define-key map (kbd "r") 'code-review-transient-api)
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c RET") 'code-review-submit-single-diff-comment-at-point)
    (define-key map (kbd "C-c C-s") 'code-review-comment-code-suggestion)
    (define-key map (kbd "G") 'code-review-reload)
    (set-keymap-parent map magit-section-mode-map)
    map))

(define-derived-mode code-review-mode magit-section-mode "Code Review"
  "Code Review mode.")

(provide 'code-review)
;;; code-review.el ends here
