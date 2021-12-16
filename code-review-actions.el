;;; code-review-actions.el --- Functions to perform some action -*- lexical-binding: t; -*-
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
;;
;;; Commentary:
;;
;;  Functions to perform some action
;;
;;; Code:

(require 'code-review-comment)
(require 'code-review-db)
(require 'code-review-gitlab)
(require 'code-review-github)
(require 'let-alist)

(defcustom code-review-always-restrict-approval? nil
  "Set to non-nil to disallow approving a PR with a bad CI state."
  :type 'boolean
  :group 'code-review)

(defcustom code-review-lgtm-message "LGTM! :thumbsup:"
  "Message to add to fast track LGTM code review."
  :group 'code-review
  :type 'string)

;; remove free variable warnings
(defvar code-review-comment-buffer-name)
(defvar code-review-commit-buffer-name)
(defvar code-review-buffer-name)

;;;
;;;; * Submit review
;;;

(defclass code-review-submit-local-coment ()
  ((path     :initarg :path)
   (position :initarg :position)
   (body     :initarg :body)
   (internal-id :initarg :internal-id)
   (line-type :initarg :line-type)))

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
  "Verify if PREVIOUS-OBJ has CURRENT-ID."
  (if (not previous-obj)
      t
    (not (-contains-p (-map (lambda (it)
                              (oref it internal-id))
                            previous-obj)
                      current-id))))

;;;###autoload
(defun code-review--submit (event &optional feedback only-reply?)
  "Submit your review with a final verdict (EVENT).
If you already have a FEEDBACK string use it.
If you want only to submit replies, use ONLY-REPLY? as non-nil."
  (interactive)
  (setq code-review-comment-cursor-pos (point))
  (let* ((pr (code-review-db-get-pullreq))
         (review-obj (cond
                      ((code-review-github-repo-p pr)
                       (code-review-submit-github-review))
                      ((code-review-gitlab-repo-p pr)
                       (code-review-submit-gitlab-review))
                      (t
                       (code-review-submit-review))))
         (replies-obj (cond
                       ((code-review-github-repo-p pr)
                        (code-review-submit-github-replies))
                       ((code-review-gitlab-repo-p pr)
                        (code-review-submit-gitlab-replies))
                       (t
                        (code-review-submit-replies)))))

    (oset review-obj state event)
    (oset review-obj pr pr)
    (oset replies-obj pr pr)

    (when feedback
      (oset review-obj feedback feedback))

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
                 (when (and (code-review-feedback-section-p section)
                            (not feedback))
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
                            :internal-id (oref value internalId)
                            :line-type (oref value line-type))
                           local-comments))))
               (forward-line))))))

      (oset replies-obj replies replies)
      (oset review-obj local-comments local-comments)

      (if (and (not (oref review-obj feedback))
               (not only-reply?)
               (not (string-equal event "APPROVE")))
          (message "You must provide a feedback msg before submit your Review.")
        (progn
          (when (not only-reply?)
            (code-review-send-review
             review-obj
             (lambda (&rest _)
               (let ((code-review-section-full-refresh? t))
                 (oset pr finished t)
                 (oset pr finished-at (current-time-string))
                 (code-review-db-update pr)
                 (code-review--build-buffer
                  code-review-buffer-name
                  nil
                  "Done submitting review")))))

          (when (oref replies-obj replies)
            (code-review-send-replies
             replies-obj
             (lambda (&rest _)
               (let ((code-review-section-full-refresh? t))
                 (code-review--build-buffer
                  code-review-buffer-name
                  nil
                  "Done submitting review and replies."))))))))))

;;;###autoload
(defun code-review-submit-approve (&optional feedback)
  "Approve current PR.
Optionally set a FEEDBACK message."
  (interactive)
  (let* ((pr (code-review-db-get-pullreq))
         (last-commit (-> (oref pr raw-infos)
                          (a-get-in (list 'commits 'nodes))
                          (-last-item))))
    (let-alist last-commit
      (cond
       ((string-equal .commit.statusCheckRollup.state "SUCCESS")
        (code-review--submit "APPROVE" feedback))
       (code-review-always-restrict-approval?
        (message "PR have CI issues. You cannot approve it."))
       (t
        (let ((res (y-or-n-p "PR have CI issues.  Do you want to proceed? ")))
          (if res
              (code-review--submit "APPROVE" feedback)
            (message "Approval process canceled."))))))))

;;;###autoload
(defun code-review-submit-comments ()
  "Comment current PR."
  (interactive)
  (code-review--submit "COMMENT"))

;;;###autoload
(defun code-review-submit-request-changes ()
  "Approve current PR."
  (interactive)
  (code-review--submit "REQUEST_CHANGES"))

;;;###autoload
(defun code-review-submit-lgtm ()
  "Submit LGTM review."
  (interactive)
  (code-review-submit-approve code-review-lgtm-message))

;;;###autoload
(defun code-review-submit-only-replies ()
  "Submit only replies."
  (interactive)
  (code-review--submit nil nil t))

;;;###autoload
(defun code-review-submit-single-top-level-comment ()
  "Add single comment without a Review."
  (interactive)
  (let ((code-review-comment-single-comment? t))
    (code-review-comment-add
     code-review-comment-single-comment-msg)))

;;;###autoload
(defun code-review-submit-single-diff-comment ()
  "Add single code comment without a Review."
  (interactive)
  (setq code-review-comment-send? t)
  (let ((code-review-comment-buffer-msg
         code-review-comment-single-comment-msg))
    (code-review-comment-add-or-edit)))

;;;
;;;; * Save/Record unfinished Review
;;;

;;;###autoload
(defun code-review-save-unfinished-review ()
  "Save unfinished review work."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr saved t)
    (oset pr saved-at (current-time-string))
    (code-review-db-update pr)
    (message "PR saved successfully!")))

;;;###autoload
(defun code-review-recover-unfinished-review (url)
  "Recover unfinished review for URL."
  (interactive "sPR URL: ")
  (let-alist (code-review-utils-pr-from-url url)
    (let ((obj (code-review-db-search .owner .repo .num)))
      (if (not obj)
          (message "No Review found for this URL.")
        (progn
          (setq code-review-db--pullreq-id (oref obj id))
          (let ((code-review-section-full-refresh? nil))
            (code-review--build-buffer)))))))

;;;###autoload
(defun code-review-open-unfinished-review ()
  "Choose an unfinished review to work on."
  (interactive)
  (let* ((objs (code-review-db-all-unfinished))
         (choice (completing-read "Unifinished Reviews: "
                                  (-map
                                   (lambda (o)
                                     (format "%s/%s - %s - %s"
                                             (oref o owner)
                                             (oref o repo)
                                             (oref o number)
                                             (oref o saved-at)))
                                   objs)))
         (obj-chosen (car (-filter
                           (lambda (o)
                             (save-match-data
                               (and (string-match "\\(.*\\)/\\(.*\\) - \\(.*\\) - \\(.*\\)" choice)
                                    (let ((owner (match-string 1 choice))
                                          (repo (match-string 2 choice))
                                          (number (match-string 3 choice))
                                          (saved-at (match-string 4 choice)))
                                      (and (string-equal (oref o owner) owner)
                                           (string-equal (oref o repo) repo)
                                           (string-equal (oref o number) number)
                                           (string-equal (oref o saved-at) saved-at))))))
                           objs))))
    (setq code-review-db--pullreq-id (oref obj-chosen id))
    (let ((code-review-section-full-refresh? nil))
      (code-review--build-buffer))))

;;;
;;;; * Merge PR
;;;

;;;###autoload
(defun code-review-merge-merge ()
  "Merge PR with MERGE strategy."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (if (code-review-github-repo-p pr)
        (progn
          (code-review-merge pr "merge")
          (oset pr state "MERGED")
          (code-review-db-update pr)
          (code-review--build-buffer))
      (code-review-gitlab-not-supported-message))))

;;;###autoload
(defun code-review-merge-rebase ()
  "Merge PR with REBASE strategy."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (if (code-review-github-repo-p pr)
        (progn
          (code-review-merge pr "rebase")
          (oset pr state "MERGED")
          (code-review-db-update pr)
          (code-review--build-buffer))
      (code-review-gitlab-not-supported-message))))

;;;###autoload
(defun code-review-merge-squash ()
  "Merge PR with SQUASH strategy."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (if (code-review-github-repo-p pr)
        (progn
          (code-review-merge pr "squash")
          (oset pr state "MERGED")
          (code-review-db-update pr)
          (code-review--build-buffer))
      (code-review-gitlab-not-supported-message))))


;;;
;;;; * Setters
;;;

;;;###autoload
(defun code-review-set-feedback ()
  "Add review FEEDBACK."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name))
        (pr (code-review-db-get-pullreq)))
    (setq code-review-comment-feedback? t)
    (setq code-review-comment-cursor-pos (point-min))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or (oref pr feedback) code-review-comment-feedback-msg))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

;;;###autoload
(defun code-review-set-title ()
  "Add review title."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name))
        (pr (code-review-db-get-pullreq)))
    (setq code-review-comment-cursor-pos (point-min))
    (setq code-review-comment-title? t)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (oref pr title))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

;;;###autoload
(defun code-review-set-label ()
  "Set label."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (when-let (options (code-review-get-labels pr))
      (let* ((choices (completing-read-multiple "Choose: " options))
             (labels (append
                      (-map (lambda (x)
                              `((name . ,x)
                                (color . "0075ca")))
                            choices)
                      (oref pr labels))))
        (setq code-review-comment-cursor-pos (point))
        (oset pr labels labels)
        (code-review-send-labels
         pr
         (lambda ()
           (code-review-db-update pr)
           (code-review--build-buffer)))))))

(defun code-review--set-assignee-field (obj &optional assignee)
  "Set assignees header field given an OBJ.
If a valid ASSIGNEE is provided, use that instead."
  (let ((candidate nil))
    (if assignee
        (setq candidate assignee)
      (when-let (options (code-review-get-assignees obj))
        (let* ((choice (completing-read "Choose: " options)))
          (setq candidate choice))))
    (oset obj assignees (list `((name) (login . ,candidate))))
    (code-review-send-assignee
     obj
     (lambda ()
       (closql-insert (code-review-db) obj t)
       (code-review--build-buffer)))))

(defun code-review-set-assignee ()
  "Set assignee."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review--set-assignee-field pr)))

(defun code-review-set-yourself-assignee ()
  "Assign yourself to PR."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review--set-assignee-field
     pr
     (code-review-utils--git-get-user))))

(defun code-review-set-milestone ()
  "Set milestone."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (if-let (options (code-review-get-milestones pr))
        (let* ((choice (completing-read "Choose: " (a-keys options)))
               (milestone `((title . ,choice)
                            (perc . 0)
                            (number .,(alist-get choice options nil nil 'equal)))))
          (setq code-review-comment-cursor-pos (point))
          (oset pr milestones milestone)
          (code-review-send-milestone
           pr
           (lambda ()
             (code-review-db-update pr)
             (code-review--build-buffer))))
      (message "No milestone found."))))

;;;###autoload
(defun code-review-set-description ()
  "Add review description."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name))
        (pr (code-review-db-get-pullreq)))
    (setq code-review-comment-description? t)
    (setq code-review-comment-cursor-pos (point-min))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (-> (oref pr raw-infos)
                  (a-get 'bodyText)))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

;;;
;;;; * Delete
;;;

;;;###autoload
(defun code-review-delete-feedback ()
  "Delete review FEEDBACK."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr feedback nil)
    (code-review-db-update pr)
    (code-review--build-buffer)))

;;;
;;;; * Miscellaneous
;;;

;;;###autoload
(defun code-review-reload ()
  "Reload the buffer."
  (interactive)
  (let* ((pr (code-review-db-get-pullreq))
         (code-review-section-full-refresh? t))
    (if pr
        (code-review--build-buffer)
      (error "No PR found"))))

;;;###autoload
(defun code-review-promote-comment-to-new-issue ()
  "Promote the comment to a new issue."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (if (code-review-gitlab-repo-p pr)
        (message "Promote comment to issue not supported in Gitlab yet.")
      (let-alist (code-review-github-promote-comment-to-new-issue-data pr)
        (-> (code-review-comment-promote-to-issue
             :reference-link .reference-link
             :author .author
             :title .title
             :body .body)
            (code-review-comment-handler-add-or-edit))))))

;;;###autoload
(defun code-review-request-reviews (&optional login)
  "Request reviewers for current PR using LOGIN if available."
  (interactive)
  (let* ((pr (code-review-db-get-pullreq))
         (users (code-review-get-assinable-users pr))
         (choices
          (if login
              (list (format "@%s :- " login))
            (completing-read-multiple
             "Request review: "
             (mapcar
              (lambda (u)
                (format "@%s :- %s" (a-get u 'login) (a-get u 'name)))
              users))))
         (logins)
         (ids (mapcar
               (lambda (choice)
                 (let* ((login (-> choice
                                   (split-string ":-")
                                   (-first-item)
                                   (split-string "@")
                                   (-second-item)
                                   (string-trim)))
                        (usr (seq-find (lambda (el)
                                         (equal (a-get el 'login) login))
                                       users)))
                   (unless usr
                     (error "User %s not found" login))
                   (setq logins
                         (append logins
                                 `(((requestedReviewer (login . ,(a-get usr 'login)))))))
                   (a-get usr 'id)))
               choices)))
    (code-review-request-review
     pr
     ids
     (lambda ()
       (let* ((infos (oref pr raw-infos))
              (new-infos
               (a-assoc-in infos (list 'reviewRequests 'nodes) logins)))
         (oset pr raw-infos new-infos)
         (code-review-db-update pr)
         (code-review--build-buffer))))))

;;;###autoload
(defun code-review-request-review-at-point ()
  "Request reviewer at point."
  (interactive)
  (setq code-review-comment-cursor-pos (point))
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (login (-> line
                    (split-string "- @")
                    (-second-item)
                    (string-trim))))
    (code-review-request-reviews login)))

;;;
;;;; * Commit actions
;;;

(defun code-review-commit-at-point ()
  "Review the current commit at point in Code Review buffer."
  (interactive)
  ;; TODO rewrite the whole feature
  ;; (let ((code-review-comment-commit-buffer? t))
  ;;   (code-review-section--build-commit-buffer
  ;;    code-review-commit-buffer-name))
  (error "Sorry, this feature became outdated and require a rewrite!"))

(defun code-review-commit-buffer-back ()
  "Move from commit buffer to review buffer."
  (interactive)
  ;; (if (equal (current-buffer)
  ;;            (get-buffer code-review-commit-buffer-name))
  ;;     (progn
  ;;       (setq code-review-comment-commit-buffer? nil
  ;;             code-review-section-full-refresh? nil)
  ;;       (kill-this-buffer)
  ;;       (code-review--trigger-hooks code-review-buffer-name))
  ;;   (message "Command must be called from Code Review Commit buffer."))
  (error "Sorry, this feature became outdated and require a rewrite!"))

;;; * Handle deprecated commands
;;; all these commands were renamed and you should use the new version

(defvar code-review-actions--renamed
  '((code-review-approve . code-review-submit-approve)
    (code-review-comments . code-review-submit-comments)
    (code-review-request-changes . code-review-submit-request-changes)
    (code-review-choose-unfinished-review . code-review-open-unfinished-review)
    (code-review-add-single-diff-comment . code-review-submit-single-diff-comment)
    (code-review-add-single-comment . code-review-submit-single-top-level-comment)
    (code-review-comment-set-feedback . code-review-set-feedback)
    (code-review--set-assignee-yourself . code-review-set-yourself-assignee)
    (code-review--set-assignee . code-review-set-assignee)
    (code-review--set-milestone . code-review-set-milestone)
    (code-review--set-label . code-review-set-label)
    (code-review-comment-set-title . code-review-set-title)
    (code-review-comment-set-description . code-review-set-description))
  "Functions renamed in release 0.0.5.")

(defun code-review--define-obsolete-fns (old-name new-fn)
  `(progn
     (defun ,old-name () (interactive) (funcall ,new-fn))
     (define-obsolete-function-alias ',old-name ',new-fn "v0.0.5")))

(defmacro code-review-set-obsolete-fns ()
  "Make functions in `code-review-actions--renamed' list obsolete."
  `(progn
     ,@(-map
        (lambda (f)
          (code-review--define-obsolete-fns (car f) (cdr f)))
        code-review-actions--renamed)))

(code-review-set-obsolete-fns)

(provide 'code-review-actions)
;;; code-review-actions.el ends here
