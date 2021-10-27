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
;; Package-Requires: ((emacs "25.1") (closql "1.2.0") (magit "3.0.0") (a "1.0.0") (ghub "3.5.1") (uuidgen "1.2") (deferred "0.5.1") (markdown-mode "2.4") (forge "0.3.0"))

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

(defcustom code-review-lgtm-message "LGTM! :thumbsup:"
  "Message to add to fast track LGTM code review."
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
  "Hook run to insert headers into the code review buffer."
  :group 'code-review
  :type 'hook)

(defcustom code-review-sections-hook
  '(code-review-section-insert-headers
    code-review-section-insert-commits
    code-review-section-insert-pr-description
    code-review-section-insert-feedback-heading
    code-review-section-insert-general-comments)
  "Hook run to insert sections into a code review buffer."
  :group 'code-review
  :type 'hook)

(defcustom code-review-sections-commit-hook
  '(code-review-section-insert-headers)
  "Hook run to insert sections into a code review commit buffer."
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

;;; buffer keymaps

(defvar magit-code-review-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-commit-at-point)
    map)
  "Keymap for the `commit' section.")

(defvar magit-code-review-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymap for the `comment' section.")

(defvar magit-code-review-comment-header-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymap for the `comment' section.")

(defvar magit-code-review-local-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-comment-delete)
    map)
  "Keymap for the `local-comment' section.")

(defvar magit-code-review-local-comment-header-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-comment-delete)
    map)
  "Keymap for the `local-comment-header' section.")

(defvar magit-code-review-feedback-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymap for the `feedback' section.")

(defvar magit-code-review-feedback-header-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymap for the `feedback' section.")

(defvar magit-code-review-labels-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review--set-label)
    map)
  "Keymap for the `label' section.")

(defvar magit-code-review-assignee-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review--set-assignee)
    map)
  "Keymap for the `assignee' section.")

(defvar magit-code-review-milestone-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review--set-milestone)
    map)
  "Keymap for the `milestone' section.")

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

(defun code-review-commit-at-point ()
  "Review the current commit at point in Code Review buffer."
  (interactive)
  (let ((section (magit-current-section)))
    (if section
        (with-slots (type value) section
          (if (eq type 'code-review-commit)
              (code-review-section--build-commit-buffer
               code-review-commit-buffer-name)
            (message "Can only be called from a commit section.")))
      (message "Can only be called from a commit section."))))

(defun code-review-commit-buffer-back ()
  "Move from commit buffer to review buffer."
  (interactive)
  (if (equal (current-buffer) (get-buffer code-review-commit-buffer-name))
      (progn
        (setq code-review-comment-commit? nil
              code-review-full-refresh? nil)
        (kill-this-buffer)
        (switch-to-buffer
         (code-review-section--trigger-hooks
          code-review-buffer-name)))
    (message "Command must be called from Code Review Commit buffer.")))

(defun code-review--set-label ()
  "Set label."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review-utils--set-label-field pr)))

(defun code-review--set-assignee ()
  "Set assignee."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review-utils--set-assignee-field pr)))

(defun code-review--set-assignee-yourself ()
  "Assign yourself to PR."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review-utils--set-assignee-field
     pr
     (code-review-utils--git-get-user))))

(defun code-review--set-milestone ()
  "Set milestone."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review-utils--set-milestone-field pr)))

;;;###autoload
(defun code-review-submit-lgtm ()
  "Submit LGTM review."
  (interactive)
  (code-review-submit "APPROVE" code-review-lgtm-message))

;;; Entrypoint

;;;###autoload
(defun code-review-start (url)
  "Start review given PR URL."
  (interactive "sPR URL: ")
  (code-review-utils-build-obj-from-url url)
  (setq code-review-full-refresh? t)
  (code-review-section--build-buffer
   code-review-buffer-name))

;;;###autoload
(defun code-review-forge-pr-at-point ()
  "Review the forge pull request at point.
OUTDATED."
  (interactive)
  (setq code-review-full-refresh? t)
  (code-review-utils--start-from-forge-at-point))

;;; Commit buffer

(define-minor-mode code-review-commit-minor-mode
  "Code Review Commit"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "b") 'code-review-commit-buffer-back)
            map))

;;; Transient

(transient-define-prefix code-review-transient-api ()
  "Code Review"
  ["Review"
   ("a" "Approve" code-review-approve)
   ("r" "Request Changes" code-review-request-changes)
   ("c" "Comment" code-review-comments)
   ("sf" "Add Feedback" code-review-comment-add-feedback)
   ("C-c C-c" "Submit" code-review-submit)]
  ["Fast track"
   ("l" "LGTM - Approved" code-review-submit-lgtm)]
  ["Setters"
   ("sy" "Set Yourself as Assignee" code-review--set-assignee-yourself)
   ("sa" "Set Assignee" code-review--set-assignee)
   ("sm" "Set Milestone" code-review--set-milestone)
   ("sl" "Set Labels" code-review--set-label)
   ("st" "Set Title" code-review-comment-add-title)
   ("sd" "Set Description" code-review-comment-add-description)]
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
