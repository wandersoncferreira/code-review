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

(defcustom code-review-new-buffer-window-strategy
  #'switch-to-buffer-other-window
  "Function used after create a new Code Review buffer."
  :group 'code-review
  :type 'function)

(defcustom code-review-log-file (expand-file-name
                                 "code-review-error.log"
                                 user-emacs-directory)
  "Path to write append only log errors."
  :group 'code-review
  :type 'file)

(defcustom code-review-headers-hook
  '(code-review-section-insert-header-title
    code-review-section-insert-title
    code-review-section-insert-state
    code-review-section-insert-ref
    code-review-section-insert-milestone
    code-review-section-insert-labels
    code-review-section-insert-assignee
    code-review-section-insert-project
    code-review-section-insert-suggested-reviewers
    code-review-section-insert-is-draft)
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

;;; Submit structure

(defclass code-review-submit-local-coment ()
  ((path     :initarg :path)
   (position :initarg :position)
   (body     :initarg :body)
   (internal-id :initarg :internal-id)))

(defclass code-review-submit-review ()
  ((state :initform nil)
   (pr :initform nil)
   (local-comments :initform nil
                   :type (satisfies
                          (lambda (it)
                            (-all-p #'code-review-submit-local-coment-p it))))
   (feedback :initform nil)))

(defclass code-review-submit-reply ()
  ((reply-to-id :initarg :reply-to-id)
   (body        :initarg :body)
   (internal-id :initarg :internal-id)))

(defclass code-review-submit-replies ()
  ((pr      :initform nil)
   (replies :initform nil
            :type (satisfies
                   (lambda (it)
                     (-all-p #'code-review-submit-reply-p it))))))

(defun code-review-submit--unique? (previous-obj current-id)
  "Verify if CURRENT-ID is in PREVIOUS-OBJ."
  (if (not previous-obj)
      t
    (not (-contains-p (-map (lambda (it)
                              (oref it internal-id))
                            previous-obj)
                      current-id))))

(cl-defmethod code-review-core-send-review ((review code-review-submit-review) callback)
  "Submit review comments given REVIEW and a CALLBACK fn."
  (let* ((pr (oref review pr))
         (payload (a-alist 'body (oref review feedback)
                           'event (oref review state)
                           'commit_id (oref pr sha)))
         (payload (if (oref review local-comments)
                      (a-assoc payload 'comments (--sort
                                                  (< (a-get it 'position)
                                                     (a-get other 'position))
                                                  (-map
                                                   (lambda (c)
                                                     `((path . ,(oref c path))
                                                       (position . ,(oref c position))
                                                       (body . ,(oref c body))))
                                                   (oref review local-comments))))
                    payload)))
    (ghub-post (format "/repos/%s/%s/pulls/%s/reviews"
                       (oref pr owner)
                       (oref pr repo)
                       (oref pr number))
               nil
               :auth 'code-review
               :payload payload
               :host code-review-github-host
               :errorback #'code-review-github-errback
               :callback callback)))

(cl-defmethod code-review-core-send-replies ((replies code-review-submit-replies) callback)
  "Submit replies to review comments inline given REPLIES and a CALLBACK fn."
  (let ((pr (oref replies pr)))
    (deferred:$
      (deferred:parallel
        (-map
         (lambda (reply)
           (lambda ()
             (ghub-post (format "/repos/%s/%s/pulls/%s/comments/%s/replies"
                                (oref pr owner)
                                (oref pr repo)
                                (oref pr number)
                                (oref reply reply-to-id))
                        nil
                        :payload (a-alist 'body (oref reply body))
                        :headers code-review-github-diffheader
                        :auth 'code-review
                        :host code-review-github-host
                        :callback (lambda (&rest _))
                        :errorback #'code-review-github-errback)))
         (oref replies replies)))

      (deferred:nextc it
        (lambda (_x)
          (prin1 "HOW MANY TIMES?\n")
          (funcall callback)))

      (deferred:error it
        (lambda (err)
          (message "Got an error from the Github Reply API %S!" err))))))

;;;###autoload
(defun code-review-submit (event &optional feedback only-reply?)
  "Submit your review with a final verdict (EVENT).
If you already have a FEEDBACK string use it.
If you want only to submit replies, use ONLY-REPLY? as non-nil."
  (interactive)
  (let ((review-obj (code-review-submit-review))
        (replies-obj (code-review-submit-replies))
        (pr (code-review-db-get-pullreq)))

    (oset review-obj state event)
    (oset review-obj pr pr)
    (oset replies-obj pr pr)

    (let ((replies nil)
          (local-comments nil))
      (with-current-buffer (get-buffer code-review-buffer-name)
        (save-excursion
          (goto-char (point-min))
          (magit-wash-sequence
           (lambda ()
             (let ((section (magit-current-section)))
               (with-slots (value) section
                 ;; get feedback
                 (when (code-review-feedback-section-p section)
                   (oset review-obj feedback (oref value msg)))

                 ;; get replies
                 (when (code-review-reply-comment-section-p section)
                   (when (code-review-submit--unique? replies (oref value internalId))
                     (push (code-review-submit-reply
                            :reply-to-id (oref value id)
                            :body (oref value msg)
                            :internal-id (oref value internalId))
                           replies)))

                 ;; get local comments
                 (when (code-review-local-comment-section-p section)
                   (when (code-review-submit--unique? local-comments (oref value internalId))
                     (push (code-review-submit-local-coment
                            :path (oref value path)
                            :position (oref value position)
                            :body (oref value msg)
                            :internal-id (oref value internalId))
                           local-comments))))
               (forward-line))))))

      (oset replies-obj replies replies)
      (oset review-obj local-comments local-comments)

      (if (and (not (oref review-obj feedback))
               (not only-reply?))
          (message "You must provide a feedback msg before submit your Review.")
        (progn
          (when (not only-reply?)
            (code-review-core-send-review
             review-obj
             (lambda (&rest _)
               (let ((code-review-section-full-refresh? t))
                 (code-review-section--build-buffer
                  code-review-buffer-name
                  nil
                  "Done submitting review")))))

          (when (oref replies-obj replies)
            (code-review-core-send-replies
             replies-obj
             (lambda (&rest _)
               (let ((code-review-section-full-refresh? t))
                 (code-review-section--build-buffer
                  code-review-buffer-name
                  nil
                  "Done submitting review and replies."))))))))))

(defun code-review-commit-at-point ()
  "Review the current commit at point in Code Review buffer."
  (interactive)
  (setq code-review-comment-commit-buffer? t)
  (code-review-section--build-commit-buffer
   code-review-commit-buffer-name))

(defun code-review-commit-buffer-back ()
  "Move from commit buffer to review buffer."
  (interactive)
  (if (equal (current-buffer)
             (get-buffer code-review-commit-buffer-name))
      (progn
        (setq code-review-comment-commit-buffer? nil
              code-review-section-full-refresh? nil)
        (kill-this-buffer)
        (code-review-section--trigger-hooks
         code-review-buffer-name))
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


;;;###autoload
(defun code-review-submit-only-replies ()
  "Submit only replies."
  (interactive)
  (code-review-submit nil nil t))

;;;###autoload
(defun code-review-merge-merge ()
  "Merge PR with MERGE strategy."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review-core-merge pr "merge")
    (oset pr state "MERGED")
    (code-review-db-update pr)
    (code-review-section--build-buffer
     code-review-buffer-name)))

;;;###autoload
(defun code-review-merge-rebase ()
  "Merge PR with REBASE strategy."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review-core-merge pr "rebase")
    (oset pr state "MERGED")
    (code-review-db-update pr)
    (code-review-section--build-buffer
     code-review-buffer-name)))

;;;###autoload
(defun code-review-merge-squash ()
  "Merge PR with SQUASH strategy."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review-core-merge pr "squash")
    (oset pr state "MERGED")
    (code-review-db-update pr)
    (code-review-section--build-buffer
     code-review-buffer-name)))

;;; Entrypoint

;;;###autoload
(defun code-review-start (url)
  "Start review given PR URL."
  (interactive "sPR URL: ")
  (setq code-review-section-full-refresh? t)
  (ignore-errors
    (code-review-utils-build-obj-from-url url)
    (code-review-section--build-buffer
     code-review-buffer-name))
  (setq code-review-section-full-refresh? nil))

;;;###autoload
(defun code-review-forge-pr-at-point ()
  "Review the forge pull request at point.
OUTDATED."
  (interactive)
  (setq code-review-section-full-refresh? t)
  (ignore-errors
    (code-review-utils--start-from-forge-at-point))
  (setq code-review-section-full-refresh? nil))

;;; Commit buffer

(define-minor-mode code-review-commit-minor-mode
  "Code Review Commit."
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "b") 'code-review-commit-buffer-back)
            map))

;;; Transient

(transient-define-prefix code-review-transient-api ()
  "Code Review"
  [["Review"
    ("a" "Approve" code-review-approve)
    ("r" "Request Changes" code-review-request-changes)
    ("c" "Comment" code-review-comments)
    ("C-c C-c" "Submit" code-review-submit)]
   ["Merge"
    ("mm" "Merge" code-review-merge-merge)
    ("mr" "Merge Rebase" code-review-merge-rebase)
    ("ms" "Merge Squash" code-review-merge-squash)]]
  ["Fast track"
   ("l" "LGTM - Approved" code-review-submit-lgtm)
   ("p" "Submit Replies" code-review-submit-only-replies)]
  ["Setters"
   ("sf" "Feedback" code-review-comment-set-feedback)
   ("sy" "Yourself as Assignee" code-review--set-assignee-yourself)
   ("sa" "Assignee" code-review--set-assignee)
   ("sm" "Milestone" code-review--set-milestone)
   ("sl" "Labels" code-review--set-label)
   ("st" "Title" code-review-comment-set-title)
   ("sd" "Description" code-review-comment-set-description)]
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
