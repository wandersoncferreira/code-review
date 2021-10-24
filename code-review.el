;;; code-review.el --- Perform code review from Github -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.1
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review
;; Package-Requires: ((emacs "25.1") (magit "3.3.0") (s "1.12.0") (ghub "2.0") (dash "2.11.0") (deferred "0.5.1") (a "0.1.1"))

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
;;; Commentary:
;;
;; `code-review` lets you submit code review with Emacs.
;;
;; Currently only supports Github but contains extension points to other forges.
;;

;;; Code:

(require 'closql)
(require 'magit-section)
(require 'code-review-section)
(require 'code-review-github)
(require 'code-review-comment)
(require 'code-review-utils)
(require 'code-review-db)
(require 'code-review-core)

(defcustom code-review-buffer-name "*Code Review*"
  "Name of the code review main buffer."
  :group 'code-review
  :type 'string)

(defcustom code-review-commit-buffer-name "*Code Review Commit*"
  "Name of the code review commit buffer."
  :group 'code-review
  :type 'string)

(defcustom code-review-headers-hook
  '(code-review-section-insert-header-title
    code-review-section-insert-title
    code-review-section-insert-state
    code-review-section-insert-ref
    code-review-section-insert-milestone
    code-review-section-insert-labels
    code-review-section-insert-assignee
    code-review-section-insert-project
    code-review-section-insert-suggested-reviewers)
  "Hook run to insert headers into the code review buffer.")

(defcustom code-review-sections-hook
  '(code-review-section-insert-headers
    code-review-section-insert-commits
    code-review-section-insert-pr-description
    code-review-section-insert-feedback-heading
    code-review-section-insert-general-comments)
  "Hook run to insert sections into a code review buffer."
  :group 'code-review
  :type 'hook)

;;; Faces

(defface code-review-recent-comment-heading
  '((((supports (:box t))) :inherit magit-branch-remote :box t)
    (t                     :inherit magit-branch-remote :inverse-video t))
  "Face for recent comments"
  :group 'code-review)

(defface code-review-outdated-comment-heading
  '((((supports (:box t))) :inherit magit-cherry-equivalent :box t)
    (t                     :inherit magit-cherry-equivalent :inverse-video t))
  "Face for outdated comments"
  :group 'code-review)

(defun code-review-commit-at-point ()
  "Review the current commit at point in Code Review buffer."
  (interactive)
  (let ((section (magit-current-section)))
    (if section
        (with-slots (type value) section
          (if (eq type 'code-review:commit)
              (code-review-section--build-commit-buffer)
            (message "Can only be called from a commit section.")))
      (message "Can only be called from a commit section."))))

;;; buffer keymaps

(defvar magit-code-review:commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-commit-at-point)
    map)
  "Keymap for the `commit' section.")

(defvar magit-code-review:comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymap for the `comment' section.")

(defvar magit-code-review:comment-header-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymap for the `comment' section.")

(defvar magit-code-review:local-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-comment-delete)
    map)
  "Keymap for the `local-comment' section.")

(defvar magit-code-review:local-comment-header-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-comment-delete)
    map)
  "Keymap for the `local-comment-header' section.")

(defvar magit-code-review:feedback-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymap for the `feedback' section.")

(defvar magit-code-review:feedback-header-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymap for the `feedback' section.")

;;; public functions

;;;###autoload
(defun code-review-approve ()
  "Approve current PR."
  (interactive)
  (code-review-submit "APPROVE"))

;;;###autoload
(defun code-review-comments ()
  "Comment current PR."
  (interactive)
  (code-review-submit "COMMENT"))

;;;###autoload
(defun code-review-request-changes ()
  "Approve current PR."
  (interactive)
  (code-review-submit "REQUEST_CHANGES"))

;;;###autoload
(defun code-review-submit (event &optional feedback)
  "Submit your review with a final veredict (EVENT).
If you already have a FEEDBACK string use it."
  (interactive)
  (let ((obj (code-review-utils--gen-submit-structure feedback)))
    (oset obj state event)
    (cond
     ((and (not (oref obj replies)) (not (oref obj feedback)))
      (message "Your review is empty."))

     ((not (oref obj feedback))
      (message "You need to provide a feedback message."))

     (t
      (progn
        (when (oref obj replies)
          (code-review-send-replies
           obj
           (lambda (&rest _)
             (message "Done submitting review replies"))))
        (code-review-send-review
         obj
         (lambda (&rest _)
           (message "Done submitting review")))
        (setq code-review-full-refresh? t)
        (code-review-section--build-buffer t))))))

;;;###autoload
(defun code-review-submit-lgtm ()
  "Submit LGTM review."
  (interactive)
  (code-review-submit "APPROVE" "LGTM! :thumbsup:"))

;;; Entrypoint


;;;###autoload
(defun code-review-start (url)
  "Start review given PR URL."
  (interactive "sPR URL: ")
  (code-review-utils-build-obj-from-url url)
  (setq code-review-full-refresh? t)
  (code-review-section--build-buffer))

;;;###autoload
(defun code-review-forge-pr-at-point ()
  "Review the forge pull request at point.
OUTDATED."
  (interactive)
  (let* ((pullreq (or (forge-pullreq-at-point) (forge-current-topic)))
         (repo    (forge-get-repository pullreq)))

    (if (not (forge-pullreq-p pullreq))
        (message "We can only review PRs at the moment. You tried on something else.")
      (let* ((pr-alist (a-alist 'owner   (oref repo owner)
                                'repo    (oref repo name)
                                'num     (oref pullreq number)
                                'url (when (forge-github-repository-p repo)
                                       "https://api.github.com"))))
        (code-review-utils-build-obj pr-alist)
        (setq code-review-full-refresh? t)
        (code-review-section--build-buffer)))))

;;; Transient

(define-transient-command code-review-transient-api ()
  "Code Review"
  ["Review"
   ("a" "Approve" code-review-approve)
   ("r" "Request Changes" code-review-request-changes)
   ("c" "Comment" code-review-comments)
   ("s" "Submit" code-review-submit)
   ("f" "Add feedback!" code-review-comment-add-feedback)]
  ["Fast track"
   ("l" "LGTM - Approved" code-review-submit-lgtm)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defvar code-review-mode-map
  (let ((map (copy-keymap magit-section-mode-map)))
    (suppress-keymap map t)
    (define-key map (kbd "r") 'code-review-transient-api)
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (set-keymap-parent map magit-section-mode-map)
    map))

(define-derived-mode code-review-mode magit-section-mode "Code Review"
  "Code Review mode")

(provide 'code-review)
;;; code-review.el ends here
