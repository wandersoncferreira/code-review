;;; code-review.el --- Perform code review from Github and Gitlab -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.3
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
;; and Transient.  Currently supports Github and Gitlab.
;;

;;; Code:

(require 'closql)
(require 'magit-section)
(require 'code-review-github)
(require 'code-review-gitlab)
(require 'code-review-render)
(require 'code-review-comment)
(require 'code-review-utils)
(require 'code-review-db)
(require 'code-review-interfaces)
(require 'code-review-faces)

(defgroup code-review nil
  "Code Review tool for VC forges."
  :group 'tools)

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


(defcustom code-review-download-dir "/tmp/code-review/"
  "Directory where code review will download binary files."
  :type 'string
  :group 'code-review)

(defcustom code-review-log-file (expand-file-name
                                 "code-review-error.log"
                                 user-emacs-directory)
  "Path to write append only log errors."
  :group 'code-review
  :type 'file)

(defcustom code-review-headers-hook
  '(code-review-render--header-title
    code-review-render--header-state
    code-review-render--header-ref
    code-review-render--header-milestone
    code-review-render--header-labels
    code-review-render--header-assignee
    code-review-render--header-project
    code-review-render--header-draft?
    code-review-render--header-suggested-reviewers
    code-review-render--header-reviewers)
  "Hook run to insert headers into the code review buffer."
  :group 'code-review
  :type 'hook)

(defcustom code-review-renders-hook
  '(code-review-render--headers
    code-review-render--commits
    code-review-render--pr-description
    code-review-render--feedback
    code-review-render--general-comments
    code-review-render--files-report)
  "Hook run to insert sections into a code review buffer."
  :group 'code-review
  :type 'hook)

(defcustom code-review-renders-commit-hook
  '(code-review-render-insert-headers)
  "Hook run to insert sections into a code review commit buffer."
  :group 'code-review
  :type 'hook)

;;; vars

(defun code-review-auth-source-debug ()
  "Do not warn on auth source search because it messes with progress reporter."
  (setq-local auth-source-debug (lambda (&rest _))))

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
          (code-review-render--build-buffer
           code-review-buffer-name))))))

;;;###autoload
(defun code-review-choose-unfinished-review ()
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
    (code-review-render--build-buffer
     code-review-buffer-name)))

;;; Submit structure

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
(defun code-review-submit (event &optional feedback only-reply?)
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
               (oset pr finished t)
               (oset pr finished-at (current-time-string))
               (code-review-db-update pr)
               (code-review-render--build-buffer
                code-review-buffer-name
                t
                nil
                "Done submitting review"))))

          (when (oref replies-obj replies)
            (code-review-send-replies
             replies-obj
             (lambda (&rest _)
               (code-review-render--build-buffer
                code-review-buffer-name
                t
                nil
                "Done submitting review and replies.")))))))))

(defun code-review-commit-at-point ()
  "Review the current commit at point in Code Review buffer."
  (interactive)
  (setq code-review-comment-commit-buffer? t)
  (code-review-render--build-commit-buffer
   code-review-commit-buffer-name))

(defun code-review-commit-buffer-back ()
  "Move from commit buffer to review buffer."
  (interactive)
  (if (equal (current-buffer)
             (get-buffer code-review-commit-buffer-name))
      (progn
        (setq code-review-comment-commit-buffer? nil
              code-review-render-full-refresh? nil)
        (kill-this-buffer)
        (code-review--trigger-hooks
         code-review-buffer-name))
    (message "Command must be called from Code Review Commit buffer.")))

(defun code-review--set-label ()
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
        (code-review-set-labels
         pr
         (lambda ()
           (code-review-db-update pr)
           (code-review-render--build-buffer
            code-review-buffer-name)))))))

(defun code-review--set-assignee-field (obj &optional assignee)
  "Helper function to set assignees header field given an OBJ.
If a valid ASSIGNEE is provided, use that instead."
  (let ((candidate nil))
    (if assignee
        (setq candidate assignee)
      (when-let (options (code-review-get-assignees obj))
        (let* ((choice (completing-read "Choose: " options)))
          (setq candidate choice))))
    (oset obj assignees (list `((name) (login . ,candidate))))
    (code-review-set-assignee
     obj
     (lambda ()
       (code-review-db-update obj)
       (code-review-render--build-buffer
        code-review-buffer-name)))))

(defun code-review--set-assignee ()
  "Set assignee."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review--set-assignee-field pr)))

(defun code-review--set-assignee-yourself ()
  "Assign yourself to PR."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (code-review--set-assignee-field
     pr
     (code-review-utils--git-get-user))))

(defun code-review--set-milestone ()
  "Set milestone."
  (interactive)
  (let ((pr (code-review-db-get-pullreq)))
    (when-let (options (code-review-get-milestones pr))
      (let* ((choice (completing-read "Choose: " (a-keys options)))
             (milestone `((title . ,choice)
                          (perc . 0)
                          (number .,(alist-get choice options nil nil 'equal)))))
        (setq code-review-comment-cursor-pos (point))
        (oset pr milestones milestone)
        (code-review-set-milestone
         pr
         (lambda ()
           (code-review-db-update pr)
           (code-review-render--build-buffer
            code-review-buffer-name)))))))

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
    (if (code-review-github-repo-p pr)
        (progn
          (code-review-merge pr "merge")
          (oset pr state "MERGED")
          (code-review-db-update pr)
          (code-review-render--build-buffer
           code-review-buffer-name t))
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
          (code-review-render--build-buffer
           code-review-buffer-name t))
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
          (code-review-render--build-buffer
           code-review-buffer-name t))
      (code-review-gitlab-not-supported-message))))

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

;;; Entrypoint

;;;###autoload
(defun code-review-start (url)
  "Start review given PR URL."
  (interactive "sPR URL: ")
  (code-review-auth-source-debug)
  (code-review-utils-build-obj-from-url url)
  (code-review-render--build-buffer
   code-review-buffer-name t))

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
    (code-review-request-review pr ids
                                (lambda ()
                                  (let* ((infos (oref pr raw-infos))
                                         (new-infos
                                          (a-assoc-in infos (list 'reviewRequests 'nodes) logins)))
                                    (oset pr raw-infos new-infos)
                                    (code-review-db-update pr)
                                    (code-review-render--build-buffer
                                     code-review-buffer-name))))))

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

;;;###autoload
(defun code-review-forge-pr-at-point ()
  "Review the forge pull request at point.
OUTDATED."
  (interactive)
  (code-review-auth-source-debug)
  (code-review-utils--build-obj-forge-at-point)
  (code-review-render--build-buffer
   code-review-buffer-name t))

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
    ("a" "Approve" code-review-approve)
    ("r" "Request Changes" code-review-request-changes)
    ("c" "Comment" code-review-comments)
    ("C-c C-s" "Save Unfinished" code-review-save-unfinished-review)
    ("C-c C-r" "Recover Unfinished" code-review-choose-unfinished-review)]
   ["Merge"
    ("m m" "Merge" code-review-merge-merge)
    ("m r" "Merge Rebase" code-review-merge-rebase)
    ("m s" "Merge Squash" code-review-merge-squash)]]
  ["Fast track"
   ("l" "LGTM - Approved" code-review-submit-lgtm)
   ("p" "Submit Replies" code-review-submit-only-replies)
   ("s c" "Single Comment, immediately sent" code-review-add-single-comment)]
  ["Setters"
   ("s f" "Feedback" code-review-comment-set-feedback)
   ("s r" "Reviewers" code-review-request-reviews)
   ("s y" "Yourself as Assignee" code-review--set-assignee-yourself)
   ("s a" "Assignee" code-review--set-assignee)
   ("s m" "Milestone" code-review--set-milestone)
   ("s l" "Labels" code-review--set-label)
   ("s t" "Title" code-review-comment-set-title)
   ("s d" "Description" code-review-comment-set-description)]
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
  "Code Review mode.")

(provide 'code-review)
;;; code-review.el ends here
