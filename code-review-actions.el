;;; code-review-actions.el --- Functions to perform some action -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.7
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
(require 'code-review-bitbucket)
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

(defun code-review--submit-feedback-required? (review-obj)
  (and (not (oref review-obj feedback))
       (not (string-equal (oref review-obj state) "APPROVE"))
       (not (and (string-equal (oref review-obj state) "COMMENT")
                 (or (code-review-github-repo-p (oref review-obj pr))
                     (code-review-gitlab-repo-p (oref review-obj pr)))))))

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
                      ((code-review-bitbucket-repo-p pr)
                       (code-review-submit-bitbucket-review))))
         (replies-obj (cond
                       ((code-review-github-repo-p pr)
                        (code-review-submit-github-replies))
                       ((code-review-gitlab-repo-p pr)
                        (code-review-submit-gitlab-replies))
                       ((code-review-bitbucket-repo-p pr)
                        (code-review-submit-bitbucket-replies)))))

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

      (if (and (code-review--submit-feedback-required? review-obj)
               (not only-reply?))
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
                  "Done submitting review and replies"))))))))))

;;;###autoload
(defun code-review-submit-approve (&optional feedback)
  "Approve current PR.
Optionally set a FEEDBACK message."
  (interactive)
  (let* ((pr (code-review-db-get-pullreq))
         (last-commit (-> (oref pr raw-infos)
                          (a-get-in (list 'commits 'nodes))
                          (-last-item))))
    (if (code-review-github-repo-p pr)
        (let-alist last-commit
          (cond
           ((or (string-equal .commit.statusCheckRollup.state "SUCCESS")
                (not .commit.statusCheckRollup))
            (code-review--submit "APPROVE" feedback))
           (code-review-always-restrict-approval?
            (message "PR have CI issues. You cannot approve it."))
           (t
            (let ((res (y-or-n-p "PR have CI issues.  Do you want to proceed? ")))
              (if res
                  (code-review--submit "APPROVE" feedback)
                (message "Approval process canceled."))))))
      (code-review--submit "APPROVE" feedback))))

;;;###autoload
(defun code-review-submit-comments ()
  "Submit a Review Comment for the current PR."
  (interactive)
  (code-review--submit "COMMENT"))

;;;###autoload
(defun code-review-submit-request-changes ()
  "Submit a Request Change for the current PR."
  (interactive)
  (code-review--submit "REQUEST_CHANGES"))

;;;###autoload
(defun code-review-submit-lgtm ()
  "Submit an Approve Review with a LGTM message."
  (interactive)
  (code-review-submit-approve code-review-lgtm-message))

;;;###autoload
(defun code-review-submit-only-replies ()
  "Submit only replies comments."
  (interactive)
  (code-review--submit nil nil t))

;;;###autoload
(defun code-review-submit-single-top-level-comment ()
  "Submit a single comment without an attached Review."
  (interactive)
  (let ((code-review-comment-single-comment? t))
    (code-review-comment-add
     code-review-comment-single-comment-msg)))

;;;###autoload
(defun code-review-submit-single-diff-comment-at-point ()
  "Submit a single diff comment without an attached Review."
  (interactive)
  (let ((section (magit-current-section)))
    (with-slots (value) section
      (if (magit-hunk-section-p section)
          (progn
            (setq code-review-comment-send? t)
            (let ((code-review-comment-buffer-msg
                   code-review-comment-single-comment-msg))
              (code-review-comment-add-or-edit)))
        (error "Single diff comment at point only available in hunk sections")))))

;;;
;;;; * Save/Record unfinished Review
;;;

;;;###autoload
(defun code-review-save-unfinished-review ()
  "Save unfinished Review."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr saved t)
    (oset pr saved-at (current-time-string))
    (code-review-db-update pr)
    (message "PR saved successfully!")))

;;;###autoload
(defun code-review-recover-unfinished-review (url)
  "Recover unfinished Review for the given URL."
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
  "Choose an unfinished Review from the previous unfinished list."
  (interactive)
  (let* ((objs (code-review-db-all-unfinished))
         (choice (completing-read "Unfinished Reviews: "
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
  "Add review FEEDBACK locally.  Required to Comment and Request Change reviews."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name))
        (pr (code-review-db-get-pullreq)))
    (setq code-review-comment-feedback? t)
    (setq code-review-comment-cursor-pos (point))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (or (oref pr feedback) code-review-comment-feedback-msg))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

;;;###autoload
(defun code-review-set-title ()
  "Change the title of current PR.  Sent immediately."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name))
        (pr (code-review-db-get-pullreq)))
    (if (or (code-review-github-repo-p pr)
            (code-review-gitlab-repo-p pr))
        (progn
          (setq code-review-comment-cursor-pos (point))
          (setq code-review-comment-title? t)
          (with-current-buffer buffer
            (erase-buffer)
            (insert (oref pr title))
            (insert ?\n)
            (switch-to-buffer-other-window buffer)
            (code-review-comment-mode)))
      (message "Not supported in %s yet."
               (cond
                ((code-review-bitbucket-repo-p pr)
                 "Bitbucket"))))))

(defun code-review--labels-to-send (pr choices)
  "The PR has a list of CHOICES for labels that should be send.
This function will make sure we clean the list of labels and/or disable all of them upstream."
  (when (not (-contains-p choices "<clean all labels>"))
    (code-review--distinct-labels
     (append
      (-map (lambda (x)
              `((name . ,x)
                (color . "0075ca")))
            choices)
      (oref pr labels)))))

;;;###autoload
(defun code-review-set-label ()
  "Change the labels of current PR.  Sent immediately.
Rewrite all current labels with the options chosen here."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (when-let (options (code-review-get-labels pr))
      (let* ((choices (completing-read-multiple "Choose: " (append
                                                            options
                                                            (list "<clean all labels>"))))
             (labels (code-review--labels-to-send pr choices)))
        (setq code-review-comment-cursor-pos (point))
        (oset pr labels labels)
        (code-review-send-labels
         pr
         (lambda ()
           (code-review-db-update pr)
           (code-review--build-buffer)))))))

(defun code-review--set-assignee-field (obj &optional assignee)
  "Change assignee header field given an OBJ.
If a valid ASSIGNEE is provided, use that instead."
  (setq code-review-comment-cursor-pos (point))
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

(defun code-review-set-assignee (&rest _)
  "Change assignee for the current PR.  Sent immediately.."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review--set-assignee-field pr)))

(defun code-review-set-yourself-assignee ()
  "Assign yourself for the current PR.  Sent immediately."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review--set-assignee-field
     pr
     (code-review-utils--git-get-user))))

(defun code-review-set-milestone ()
  "Change the milestone for the current PR.  Sent immediately."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (if (code-review-github-repo-p pr)
        (progn
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
            (message "No milestone found.")))
      (message "Not supported in %s yet."
               (cond
                ((code-review-gitlab-repo-p pr)
                 "Gitlab")
                ((code-review-bitbucket-repo-p pr)
                 "Bitbucket"))))))

;;;###autoload
(defun code-review-set-description ()
  "Submit new PR description.  Sent immediately."
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
  "Delete review FEEDBACK locally."
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
  "Reload the buffer.  All your local comments will be lost."
  (interactive)
  (let* ((pr (code-review-db-get-pullreq))
         (code-review-section-full-refresh? t))
    (setq code-review-comment-cursor-pos (point))
    (if pr
        (progn
          (let ((choice (y-or-n-p "You will lose all your local comments.  Do you need to proceed? ")))
            (when choice
              (code-review--build-buffer))))
      (error "No PR found"))))

;;;###autoload
(defun code-review-promote-comment-at-point-to-new-issue ()
  "Promote comment at point to a new issue.  Sent immediately."
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
         (users (code-review-get-assignable-users pr))
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
(defun code-review-request-review-at-point (&rest _)
  "Request reviewer at point."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (if (code-review-github-repo-p pr)
        (progn
          (setq code-review-comment-cursor-pos (point))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
                 (login (-> line
                            (split-string "- @")
                            (-second-item)
                            (string-trim))))
            (code-review-request-reviews login)))
      (message "Not supported in %s yet."
               (cond
                ((code-review-gitlab-repo-p pr)
                 "Gitlab")
                ((code-review-bitbucket-repo-p pr)
                 "Bitbucket"))))))

;;;###autoload
(defun code-review-toggle-display-all-comments ()
  "Toggle display comments."
  (interactive)
  (let ((flag (not code-review-section--display-all-comments)))
    (setq code-review-section--display-all-comments flag
          code-review-section--display-diff-comments flag
          code-review-section--display-top-level-comments flag))
  (code-review--build-buffer))

;;;###autoload
(defun code-review-toggle-display-top-level-comments ()
  "Toggle display the top level comments."
  (interactive)
  (setq code-review-section--display-top-level-comments
        (not code-review-section--display-top-level-comments))
  (code-review--build-buffer))

;;;###autoload
(defun code-review-toggle-display-diff-comments ()
  "Toggle display the top level comments."
  (interactive)
  (setq
   code-review-section--display-all-comments t
   code-review-section--display-diff-comments
   (not code-review-section--display-diff-comments))
  (code-review--build-buffer))

;;;###autoload
(defun code-review-comment-code-suggestion ()
  "Add code suggestion box."
  (interactive)
  (let ((section (magit-current-section))
        (pr (code-review-db-get-pullreq)))
    (if (code-review-github-repo-p pr)
        (progn
          (setq code-review-comment-cursor-pos (point))
          (with-slots (value type) section
            (if (string-equal type "hunk")
                (code-review-comment-add-or-edit t)
              (message "Suggested not supported in this section."))))
      (message "Not supported in %s yet."
               (cond
                ((code-review-gitlab-repo-p pr)
                 "Gitlab")
                ((code-review-bitbucket-repo-p pr)
                 "Bitbucket"))))))

;;;###autoload
(defun code-review-comment-jump-next ()
  "Go to next comment in the buffer."
  (interactive)
  (let ((initial-point (point)))
    (with-current-buffer (get-buffer code-review-buffer-name)
      (let ((comment-position))
        (save-excursion
          (forward-line)
          (while (not (or (eobp)
                          (looking-at
                           "Comment by\\|Reviewed by\\|Reply by")))
            (forward-line))
          (beginning-of-line)
          (setq comment-position (point)))
        (goto-char comment-position)
        (when (equal initial-point comment-position)
          (message "No more comments going forward."))))))

;;;###autoload
(defun code-review-comment-jump-previous ()
  "Go to previous comment in the buffer."
  (interactive)
  (let ((initial-point (point)))
    (with-current-buffer (get-buffer code-review-buffer-name)
      (let ((comment-position))
        (save-excursion
          (forward-line -1)
          (while (not (or (bobp)
                          (looking-at
                           "Comment by\\|Reviewed by\\|Reply by")))
            (forward-line -1))
          (beginning-of-line)
          (setq comment-position (point)))
        (goto-char comment-position)
        (when (equal initial-point comment-position)
          (message "No more comments going backward."))))))

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

(eval-and-compile
  (defvar code-review-actions--renamed
    '((code-review-approve . code-review-submit-approve)
      (code-review-comments . code-review-submit-comments)
      (code-review-request-changes . code-review-submit-request-changes)
      (code-review-choose-unfinished-review . code-review-open-unfinished-review)
      (code-review-add-single-diff-comment . code-review-submit-single-diff-comment-at-point)
      (code-review-submit-single-diff-comment . code-review-submit-single-diff-comment-at-point)
      (code-review-add-single-comment . code-review-submit-single-top-level-comment)
      (code-review-comment-set-feedback . code-review-set-feedback)
      (code-review--set-assignee-yourself . code-review-set-yourself-assignee)
      (code-review--set-assignee . code-review-set-assignee)
      (code-review--set-milestone . code-review-set-milestone)
      (code-review--set-label . code-review-set-label)
      (code-review-comment-set-title . code-review-set-title)
      (code-review-comment-set-description . code-review-set-description)
      (code-review-promote-comment-to-new-issue . code-review-promote-comment-at-point-to-new-issue))
    "Functions renamed in release 0.0.5."))

(defmacro code-review-set-obsolete-fns ()
  "Make functions in `code-review-actions--renamed' list obsolete."
  `(progn
     ,@(-map
        (pcase-lambda (`(,old-name . ,new-name))
          `(define-obsolete-function-alias ',old-name ',new-name "v0.0.5"))
        code-review-actions--renamed)))

(code-review-set-obsolete-fns)

(provide 'code-review-actions)
;;; code-review-actions.el ends here
