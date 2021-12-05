;;; code-review-render.el --- UI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.1
;; Homepage: https://github.com/wandersoncferreira/code-review
;; Package-Requires: ((emacs "25.1"))
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
;;  Code to build the UI.
;;
;;; Code:

(require 'deferred)
(require 'magit-section)
(require 'magit-diff)
(require 'code-review-interfaces)
(require 'code-review-db)
(require 'code-review-utils)
(require 'code-review-github)
(require 'code-review-gitlab)
(require 'code-review-faces)
(require 'emojify)

(defun code-review--propertize-keyword (str)
  "Add property face to STR."
  (propertize str 'face
              (cond
               ((member str '("MERGED" "SUCCESS" "COMPLETED" "APPROVED" "REJECTED"))
                'code-review-success-state-face)
               ((member str '("FAILURE" "TIMED_OUT" "ERROR" "CHANGES_REQUESTED" "CLOSED" "CONFLICTING"))
                'code-review-error-state-face)
               ((member str '("RESOLVED" "OUTDATED"))
                'code-review-info-state-face)
               ((member str '("PENDING"))
                'code-review-pending-state-face)
               (t
                'code-review-state-face))))

;; fix unbound symbols
(defvar magit-root-section)
(defvar code-review-buffer-name)
(defvar code-review-commit-buffer-name)
(defvar code-review-comment-commit-buffer?)
(defvar code-review-fill-column)
(defvar code-review-comment-cursor-pos)
(defvar code-review-reaction-types)

(declare-function code-review-promote-comment-to-new-issue "code-review")
(declare-function code-review-utils--visit-binary-file-at-remote "code-review-utils")
(declare-function code-review-utils--visit-binary-file-at-point "code-review-utils")

(defvar code-review-render-grouped-comments nil
  "Hold grouped comments to avoid computation on every hunk line.
For internal usage only.")

(defvar code-review-render-hold-written-comment-ids nil
  "List to hold written comments ids.
For internal usage only.")

(defvar code-review-render-hold-written-comment-count nil
  "List of number of lines of comments written in the buffer.
For internal usage only.")

;; * Class Definitions

(defclass code-review-is-draft-section (magit-section)
  ((draft? :initform nil
           :type (or null string))))

(defclass code-review-title-section (magit-section)
  ((keymap  :initform 'code-review-title-section-map)
   (title  :initform nil
           :type (or null string))))

(defclass code-review-state-section (magit-section)
  ((state  :initform nil
           :type (or null string))))

(defclass code-review-ref-section (magit-section)
  ((base   :initform nil
           :type (or null string))
   (head   :initform nil
           :type (or null string))))

(defclass code-review-milestone-section (magit-section)
  ((keymap :initform 'code-review-milestone-section-map)
   (title  :initarg :title)
   (perc   :initarg :perc)
   (number :initarg :number
           :type number)))

(defclass code-review-description-section (magit-section)
  ((keymap :initform 'code-review-description-section-map)
   (id     :initarg :id)
   (msg    :initarg :msg)
   (reactions :initarg :reactions)))

(defclass code-review-feedback-section (magit-section)
  ((keymap :initform 'code-review-feedback-section-map)
   (msg    :initarg :msg)))

(defclass code-review-commit-section (magit-section)
  ((keymap :initform 'code-review-commit-section-map)
   (sha    :initarg :sha)
   (msg    :initarg :msg)))

(defclass code-review-comment-section (magit-section)
  ((keymap :initform 'code-review-comment-section-map)
   (author :initarg :author
           :type string)
   (msg    :initarg :msg
           :type string)
   (id     :initarg :id)
   (reactions :initarg :reactions)
   (typename :initarg :typename)
   (face   :initform 'magit-log-author)))

(defclass code-review-reaction-section ()
  ((id :initarg :id)
   (content :initarg :content)))

(defclass code-review-reactions-section (magit-section)
  ((context-name :initarg :context-name)
   (comment-id :initarg :comment-id)
   (reactions :initarg :reactions
              :type (satisfies
                     (lambda (it)
                       (-all-p #'code-review-reaction-section-p it))))))

(defclass code-review-base-comment-section (magit-section)
  ((state      :initarg :state
               :type string)
   (author     :initarg :author
               :type string)
   (msg        :initarg :msg
               :type string)
   (position   :initarg :position
               :type number)
   (reactions  :initarg :reactions
               :type (or null
                        (satisfies
                         (lambda (it)
                           (-all-p #'code-review-reaction-section-p it)))))
   (path       :initarg :path
               :type string)
   (diffHunk   :initarg :diffHunk
               :type (or null string))
   (id         :initarg :id
               :documentation "ID that identifies the comment in the Forge.")
   (internalId :initarg :internalId)
   (amount-loc :initform nil)
   (outdated?  :initform nil
               :type boolean)
   (reply?     :initform nil
               :type boolean)
   (local?     :initform nil
               :type boolean)
   (createdAt  :initarg :createdAt)
   (updatedAt  :initarg :updatedAt)))

(defclass code-review-code-comment-section (code-review-base-comment-section)
  ((keymap     :initform 'code-review-code-comment-section-map)
   (diffHunk   :initarg :diffHunk)
   (id         :initarg :id
               :documentation "ID that identifies the comment in the Forge.")
   (amount-loc :initform nil)
   (outdated?  :initform nil
               :type boolean)
   (reply?     :initform nil
               :type boolean)
   (local?     :initform nil
               :type boolean)))

(defclass code-review-local-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-local-comment-section-map)
   (local?       :initform t)
   (reply?       :initform nil)
   (edit?        :initform nil)
   (outdated?    :initform nil)
   (heading-face :initform 'code-review-recent-comment-heading)
   (body-face    :initform nil)
   (diffHunk     :initform nil)
   (line-type    :initarg :line-type)))

(defclass code-review-reply-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-reply-comment-section-map)
   (reply?       :initform t)
   (local?       :initform t)
   (edit?        :initform nil)
   (outdated?    :initform nil)
   (heading-face :initform 'code-review-recent-comment-heading)
   (body-face    :initform nil)))

(defclass code-review-outdated-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-outdated-comment-section-map)
   (local?       :initform t)
   (outdated?    :initform t)))

(defclass code-review-labels-section (magit-section)
  ((keymap :initform 'code-review-labels-section-map)
   (labels :initarg :labels)))

(defclass code-review-assignees-section (magit-section)
  ((keymap :initform 'code-review-assignees-section-map)
   (assignees :initarg :assignees)))

(defclass code-review-suggested-reviewers-section (magit-section)
  ((keymap :initform 'code-review-suggested-reviewers-section-map)))

(defclass code-review-check-section (magit-section)
  ((details :initarg :details)))

(defclass code-review-binary-file-section (magit-section)
  ((keymap :initform 'code-review-binary-file-section-map)))

;; * Keymaps Definitions

(defvar code-review-title-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-set-title)
    map)
  "Keymaps for code-comment sections.")

(defvar code-review-milestone-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review--set-milestone)
    map)
  "Keymaps for milestone section.")

(defvar code-review-feedback-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-set-feedback)
    (define-key map (kbd "C-c C-k") 'code-review-comment-delete-feedback)
    (define-key map (kbd "k") 'code-review-comment-delete-feedback)
    map)
  "Keymaps for feedback section.")

(defvar code-review-description-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'code-review-description-reaction-at-point)
    map)
  "Keymaps for description section.")

(defvar code-review-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-commit-at-point)
    map)
  "Keymaps for commit section.")

(defvar code-review-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'code-review-conversation-reaction-at-point)
    (define-key map (kbd "C-c C-n") 'code-review-promote-comment-to-new-issue)
    map)
  "Keymaps for comment section.")

(defvar code-review-code-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-r") 'code-review-code-comment-reaction-at-point)
    (define-key map (kbd "C-c C-n") 'code-review-promote-comment-to-new-issue)
    map)
  "Keymaps for code-comment sections.")

(defvar code-review-local-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-render-delete-comment)
    (define-key map (kbd "k") 'code-review-render-delete-comment)
    map)
  "Keymaps for local-comment sections.")

(defvar code-review-reply-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-render-delete-comment)
    (define-key map (kbd "k") 'code-review-render-delete-comment)
    map)
  "Keymaps for reply-comment sections.")

(defvar code-review-outdated-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymaps for outdated-comment sections.")

(defvar code-review-labels-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review--set-label)
    map)
  "Keymaps for code-comment sections.")

(defvar code-review-assignees-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review--set-assignee)
    map)
  "Keymaps for code-comment sections.")

(defvar code-review-suggested-reviewers-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-request-review-at-point)
    map)
  "Keymaps for suggested reviewers section.")

(defvar code-review-binary-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-utils--visit-binary-file-at-point)
    (define-key map (kbd "C-c C-v") 'code-review-utils--visit-binary-file-at-remote)
    map)
  "Keymaps for binary files sections.")

;; * Functions
;;; * Header Sections

(defun code-review-render--header-title ()
  "Insert the title header line."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (setq header-line-format
            (propertize
             (format "#%s: %s".number (code-review-db--pullreq-title))
             'font-lock-face
             'magit-section-heading)))))

(defun code-review-render--header-state ()
  "Insert the state of the header buffer."
  (when-let (state (code-review-db--pullreq-state))
    (let ((value (if state state "none")))
      (magit-insert-section (code-review-state-section value)
        (insert (format "%-17s" "State: ") value)
        (insert ?\n)))))

(defun code-review-render--header-ref ()
  "Insert the state of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let ((obj (code-review-ref-section)))
        (oset obj base .baseRefName)
        (oset obj head .headRefName)
        (magit-insert-section (code-review-ref-section obj)
          (insert (format "%-17s" "Refs: "))
          (insert .baseRefName)
          (insert (propertize " ... " 'font-lock-face 'magit-dimmed))
          (insert .headRefName)
          (insert ?\n))))))

(defun code-review-render--format-milestone (milestone)
  "Get the pretty version of MILESTONE."
  (cond
   ((and (oref milestone title) (oref milestone perc))
    (format "%s (%s%%)"
            (oref milestone title)
            (oref milestone perc)))
   ((oref milestone title)
    (oref milestone title))
   (t
    "No milestone")))

(defun code-review-render--header-milestone ()
  "Insert the milestone of the header buffer."
  (let ((milestones (code-review-db--pullreq-milestones)))
    (let-alist milestones
      (let* ((title (when (not (string-empty-p .title)) .title))
             (obj (code-review-milestone-section :title title :perc .perc)))
        (magit-insert-section (code-review-milestone-section obj)
          (insert (format "%-17s" "Milestone: "))
          (insert (propertize (code-review-render--format-milestone obj) 'font-lock-face 'magit-dimmed))
          (insert ?\n))))))

(defun code-review-render--format-label (label)
  "Add overlay and font face to a LABEL."
  (let-alist label
    (let* ((color (if (string-prefix-p "#" .color)
                      .color
                    (concat "#" .color)))
           (background (code-review-utils--sanitize-color color))
           (foreground (code-review-utils--contrast-color color))
           (o (make-overlay (- (point) (length .name)) (point))))
      (overlay-put o 'priority 2)
      (overlay-put o 'evaporate t)
      (overlay-put o 'font-lock-face
                   `((:background ,background)
                     (:foreground ,foreground)
                     forge-topic-label)))))

(defun code-review-render--header-labels ()
  "Insert the labels of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let* ((labels (-distinct
                    (append (code-review-db--pullreq-labels)
                            (a-get-in infos (list 'labels 'nodes)))))
           (obj (code-review-labels-section :labels labels)))
      (magit-insert-section (code-review-labels-section obj)
        (insert (format "%-17s" "Labels: "))
        (if labels
            (dolist (label labels)
              (insert (a-get label 'name))
              (code-review-render--format-label label)
              (insert " "))
          (insert (propertize "None yet" 'font-lock-face 'magit-dimmed)))
        (insert ?\n)))))

(defun code-review-render--header-assignee ()
  "Insert the assignee of the header buffer."
  (when-let (infos (code-review-db--pullreq-assignees))
    (let* ((assignee-names (-map
                            (lambda (a)
                              (format "%s (@%s)"
                                      (a-get a 'name)
                                      (a-get a 'login)))
                            infos))
           (assignees (if assignee-names
                          (string-join assignee-names ", ")
                        (propertize "No one — Assign yourself" 'font-lock-face 'magit-dimmed)))
           (obj (code-review-assignees-section :assignees assignees)))
      (magit-insert-section (code-review-assignees-section obj)
        (insert (format "%-17s" "Assignees: ") assignees)
        (insert ?\n)))))

(defun code-review-render--header-project ()
  "Insert the project of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((project-names (-map
                             (lambda (p)
                               (a-get-in p (list 'project 'name)))
                             .projectCards.nodes))
             (projects (if project-names
                           (string-join project-names ", ")
                         (propertize "None yet" 'font-lock-face 'magit-dimmed))))
        (magit-insert-section (code-review-project-section projects)
          (insert (format "%-17s" "Projects: ") projects)
          (insert ?\n))))))

(defun code-review-render--header-draft? ()
  "Insert the isDraft value of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let ((draft? (if .isDraft "true" "false")))
        (magit-insert-section (code-review-is-draft-section draft?)
          (insert (format "%-17s" "Draft: "))
          (insert draft?)
          (let ((o (make-overlay (- (point) (length draft?)) (point))))
            (overlay-put o 'priority 2)
            (overlay-put o 'evaporate t)
            (overlay-put o 'font-lock-face
                         `((:background ,(if .isDraft "green" "red"))
                           (:foreground ,"white")
                           forge-topic-label)))
          (insert ?\n))))))

(defun code-review-render--header-suggested-reviewers ()
  "Insert the suggested reviewers."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((reviewers-group (code-review-utils--fmt-reviewers infos))
             (reviewers (->> .suggestedReviewers
                             (-map
                              (lambda (r)
                                (a-get-in r (list 'reviewer 'login))))
                             (-filter
                              (lambda (r)
                                (let* ((res nil))
                                  (maphash
                                   (lambda (_status users)
                                     (setq res (append res
                                                       (-map
                                                        (lambda (it)
                                                          (a-get it 'login))
                                                        users))))
                                   reviewers-group)
                                  (and (not (equal r nil))
                                       (not (-contains-p res r))))))))
             (suggested-reviewers (if (not reviewers)
                                      (propertize "No suggestions" 'font-lock-face 'magit-dimmed)
                                    reviewers)))
        (magit-insert-section (code-review-suggested-reviewers-section suggested-reviewers)
          (insert "Suggested-Reviewers:")
          (if (not reviewers)
              (insert " " suggested-reviewers)
            (dolist (sr suggested-reviewers)
              (insert ?\n)
              (insert (propertize "Request Review" 'face 'code-review-request-review-face))
              (insert " - ")
              (insert (propertize (concat "@" sr) 'face 'code-review-author-face))))
          (insert ?\n))))))

(defun code-review-render--header-reviewers ()
  "Insert the reviewers section."
  (let* ((infos (code-review-db--pullreq-raw-infos))
         (groups (code-review-utils--fmt-reviewers infos)))
    (magit-insert-section (code-review-reviewers-section)
      (insert "Reviewers:\n")
      (maphash (lambda (status users-objs)
                 (dolist (user-o users-objs)
                   (let-alist user-o
                     (insert (code-review--propertize-keyword status))
                     (insert " - ")
                     (insert (propertize (concat "@" .login) 'face 'code-review-author-face))
                     (when .code-owner?
                       (insert " as CODE OWNER"))
                     (when .at
                       (insert " " (propertize (code-review-utils--format-timestamp .at) 'face 'code-review-timestamp-face))))
                   (insert ?\n)))
               groups)
      (insert ?\n))))

(defun code-review-render--headers ()
  "Insert all the headers."
  (magit-insert-headers 'code-review-headers-hook))
;;; * Commit Section

(defun code-review-render--commits ()
  "Insert commits from PULL-REQUEST."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review-commits-header-section)
        (insert (propertize "Commits" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (magit-insert-section (code-review-commits-body-section)
          (dolist (c .commits.nodes)
            (let* ((sha (a-get-in c (list 'commit 'abbreviatedOid)))
                   (msg (a-get-in c (list 'commit 'message)))
                   (obj (code-review-commit-section :sha sha :msg msg)))
              (magit-insert-section (code-review-commit-section obj)
                (insert (propertize (format "%-6s " (oref obj sha)) 'font-lock-face 'magit-hash))
                (insert (oref obj msg))))
            (insert ?\n)))
        (insert ?\n)))))
;;; * PR Description Section

(defun code-review-render--pr-description ()
  "Insert PULL-REQUEST description."
  (when-let (description (code-review-db--pullreq-description))
    (let-alist (code-review-db--pullreq-raw-infos)
      (let* ((description-cleaned (if (string-empty-p description)
                                      "No description provided."
                                    description))
             (reaction-objs (-map
                             (lambda (r)
                               (code-review-reaction-section
                                :id (a-get r 'id)
                                :content (a-get r 'content)))
                             .reactions.nodes))
             (obj (code-review-description-section :msg description-cleaned
                                                   :id .databaseId
                                                   :reactions reaction-objs)))
        (magit-insert-section (code-review-description-section obj)
          (insert (propertize "Description" 'font-lock-face 'magit-section-heading))
          (magit-insert-heading)
          (magit-insert-section (code-review-description-section obj)
            (if (string-empty-p description)
                (insert (propertize description-cleaned 'font-lock-face 'magit-dimmed))
              (insert description-cleaned))
            (insert ?\n)
            (when .reactions.nodes
              (code-review-comment-insert-reactions
               reaction-objs
               "pr-description"
               .databaseId))
            (insert ?\n)))))))

;;;; Reactions

(defun code-review-description-reaction-at-point ()
  "Toggle reaction in description sections."
  (interactive)
  (let* ((section (magit-current-section))
         (comment-id (oref (oref section value) id)))
    (code-review-toggle-reaction-at-point comment-id "pr-description")))

(defun code-review-description-add-reaction (node-id content)
  "Add NODE-ID with CONTENT in pr description."
  (let* ((pr (code-review-db-get-pullreq))
         (infos (oref pr raw-infos))
         (reactions (cons (a-alist 'content (upcase content) 'id node-id)
                          (a-get-in infos (list 'reactions 'nodes)))))
    (setf (alist-get 'reactions infos) (a-alist 'nodes reactions))
    (oset pr raw-infos infos)
    (code-review-db-update pr)))

(defun code-review-description-delete-reaction (node-id)
  "Delete NODE-ID from pr description."
  (let* ((pr (code-review-db-get-pullreq))
         (infos (oref pr raw-infos))
         (reactions (a-get-in infos (list 'reactions 'nodes)))
         (new-reactions (-filter
                         (lambda (it)
                           (not (string-equal node-id (a-get it 'id))))
                         reactions)))
    (setf (alist-get 'reactions infos) (a-alist 'nodes new-reactions))
    (oset pr raw-infos infos)
    (code-review-db-update pr)))
;;; * Feedback Section

(defun code-review-render--feedback ()
  "Insert feedback heading."
  (let* ((feedback (code-review-db--pullreq-feedback))
         (obj (code-review-feedback-section :msg feedback)))
    (magit-insert-section (code-review-feedback-section obj)
      (insert (propertize "Your Review Feedback" 'font-lock-face 'magit-section-heading))
      (magit-insert-heading)
      (magit-insert-section (code-review-feedback-section obj)
        (if feedback
            (insert feedback)
          (insert (propertize "Leave a comment here." 'font-lock-face 'magit-dimmed))))
      (insert ?\n)
      (insert ?\n))))
;;; * General Comments Section

(cl-defmethod code-review--insert-general-comment-section ((github code-review-github-repo))
  "Function to insert conversation section for GITHUB PRs."
  (let-alist (oref github raw-infos)
    (dolist (c (append .comments.nodes (-filter
                                        (lambda (n)
                                          (not
                                           (string-empty-p (a-get n 'bodyText))))
                                        .reviews.nodes)))
      (let* ((reactions (a-get-in c (list 'reactions 'nodes)))
             (reaction-objs (when reactions
                              (-map
                               (lambda (r)
                                 (code-review-reaction-section
                                  :id (a-get r 'id)
                                  :content (a-get r 'content)))
                               reactions)))
             (obj (code-review-comment-section
                   :author (a-get-in c (list 'author 'login))
                   :msg (a-get c 'bodyText)
                   :id (a-get c 'databaseId)
                   :typename (a-get c 'typename)
                   :reactions reaction-objs)))
        (magit-insert-section (code-review-comment-section obj)
          (insert (concat
                   (propertize (format "@%s" (oref obj author)) 'font-lock-face (oref obj face))
                   " - "
                   (propertize (code-review-utils--format-timestamp (a-get c 'createdAt)) 'face 'code-review-timestamp-face)))
          (magit-insert-heading)
          (code-review-insert-comment-lines obj)

          (when reactions
            (code-review-comment-insert-reactions
             reaction-objs
             "comment"
             (a-get c 'databaseId)))
          (insert ?\n))))))

(cl-defmethod code-review--insert-general-comment-section ((gitlab code-review-gitlab-repo))
  "Function to insert conversation section for GITLAB PRs."
  (let-alist (oref gitlab raw-infos)
    (let ((thread-groups (-group-by
                          (lambda (it)
                            (a-get-in it (list 'discussion 'id)))
                          .comments.nodes)))
      (dolist (g (a-keys thread-groups))
        (magit-insert-section (code-review-comment-thread-section)
          (insert (propertize "New Thread" 'font-lock-face 'code-review-thread-face))
          (magit-insert-heading)
          (let ((thread-comments (alist-get g thread-groups nil nil 'equal)))
            (dolist (c thread-comments)
              (let* ((obj (code-review-comment-section
                           :author (a-get-in c (list 'author 'login))
                           :msg (a-get c 'bodyText)
                           :id (a-get c 'databaseId))))
                (magit-insert-section (code-review-comment-section obj)
                  (insert (concat
                           (propertize (format "@%s" (oref obj author)) 'font-lock-face (oref obj face))
                           " - "
                           (propertize (code-review-utils--format-timestamp (a-get c 'createdAt)) 'face 'code-review-timestamp-face)))
                  (magit-insert-heading)
                  (code-review-insert-comment-lines obj)
                  (insert ?\n))))))))))

(defun code-review-render--general-comments ()
  "Insert general comments for the PULL-REQUEST in the buffer."
  (when-let (pr (code-review-db-get-pullreq))
    (magit-insert-section (code-review-comment-header-section)
      (insert (propertize "Conversation" 'font-lock-face 'magit-section-heading))
      (magit-insert-heading)
      (code-review--insert-general-comment-section pr))))


;;;; Reactions

(defun code-review-general-comment-reaction-at-point ()
  "Toggle reaction in general-comment sections."
  (interactive)
  (let* ((section (magit-current-section))
         (comment-id (oref (oref section value) id)))
    (setq code-review-comment-cursor-pos (point))
    (code-review-toggle-reaction-at-point comment-id "comment")))

(defun code-review-general-comment--add-or-delete-reaction (comment-id reaction-id content &optional delete?)
  "Add or Delete REACTION-ID in COMMENT-ID given a CONTENT.
Optionally DELETE? flag must be set if you want to remove it."
  (let* ((pr (code-review-db-get-pullreq))
         (infos (oref pr raw-infos))
         (update-comment (lambda (c)
                           (let-alist c
                             (when (equal .databaseId comment-id)
                               (let ((reactions-nodes (if delete?
                                                          (-filter (lambda (it)
                                                                     (not (string-equal (a-get it 'id) reaction-id)))
                                                                   .reactions.nodes)
                                                        (append .reactions.nodes
                                                                (list (a-alist 'id reaction-id
                                                                               'content (upcase content)))))))
                                 (setf (alist-get 'reactions c) (a-alist 'nodes reactions-nodes)))))
                           c)))
    (let-alist infos
      (let ((new-comments
             (-map (lambda (c) (funcall update-comment c)) .comments.nodes))
            (new-comments-review
             (-map (lambda (c) (funcall update-comment c)) .reviews.nodes)))
        (setf (alist-get 'comments infos) (a-alist 'nodes new-comments))
        (setf (alist-get 'reviews infos) (a-alist 'nodes new-comments-review))
        (oset pr raw-infos infos)
        (code-review-db-update pr)))))

(defun code-review-general-comment-add-reaction (comment-id reaction-id content)
  "Add REACTION-ID with CONTENT in PR COMMENT-ID."
  (code-review-general-comment--add-or-delete-reaction comment-id reaction-id content))

(defun code-review-general-comment-delete-reaction (comment-id reaction-id)
  "Delete REACTION-ID from COMMENT-ID."
  (code-review-general-comment--add-or-delete-reaction comment-id reaction-id nil t))
;;; * Files Report Section

(defun code-review-render--files-report ()
  "Insert files changed, added, deleted in the PR."
  (when-let (files (a-get (code-review-db--pullreq-raw-infos) 'files))
    (let-alist files
      (magit-insert-section (code-review-files-report-section)
        (insert (propertize (format "Files changed (%s files; %s additions, %s deletions)"
                                    (length .nodes)
                                    (apply #'+ (mapcar (lambda (x) (alist-get 'additions x)) .nodes))
                                    (apply #'+ (mapcar (lambda (x) (alist-get 'deletions x)) .nodes)))
                            'font-lock-face
                            'magit-section-heading))
        (magit-insert-heading)))))
;;; * Reactions

(defun code-review--toggle-reaction-at-point (pr context-name comment-id existing-reactions reaction)
  "Given a PR, use the CONTEXT-NAME to toggle REACTION in COMMENT-ID considering EXISTING-REACTIONS."
  (let* ((res (code-review-set-reaction pr context-name comment-id reaction))
         (reaction-id (a-get res 'id))
         (node-id (a-get res 'node_id))
         (existing-reaction-ids (when existing-reactions
                                  (-map (lambda (r) (oref r id)) existing-reactions))))
    (if (-contains-p existing-reaction-ids node-id)
        (progn
          (code-review-delete-reaction pr context-name comment-id reaction-id)
          (pcase context-name
            ("pr-description" (code-review-description-delete-reaction node-id))
            ("comment" (code-review-general-comment-delete-reaction comment-id node-id))
            ("code-comment" (code-review-code-comment-delete-reaction comment-id node-id))))
      (pcase context-name
        ("pr-description"
         (code-review-description-add-reaction node-id reaction))
        ("comment"
         (code-review-general-comment-add-reaction comment-id node-id reaction))
        ("code-comment"
         (code-review-code-comment-add-reaction comment-id node-id reaction))))
    (code-review-render--build-buffer code-review-buffer-name)))

(defun code-review-toggle-reaction-at-point (comment-id context-name)
  "Add reaction at point given a COMMENT-ID and CONTEXT-NAME."
  (let* ((allowed-reactions (-map
                             (lambda (it)
                               `(,(cdr it) . ,(car it)))
                             code-review-reaction-types))
         (choice (emojify-completing-read "Reaction: "
                                          (lambda (string-display)
                                            (let ((prefix (car (split-string string-display " -"))))
                                              (-contains-p (a-keys allowed-reactions) prefix)))))
         (pr (code-review-db-get-pullreq))
         (reaction (downcase (alist-get choice allowed-reactions nil nil 'equal))))
    (with-slots (value) (magit-current-section)
      (code-review--toggle-reaction-at-point
       pr
       context-name
       comment-id
       (oref value reactions)
       reaction))))

(defun code-review-reactions-reaction-at-point ()
  "Endorse or remove your reaction at point."
  (interactive)
  (setq code-review-comment-cursor-pos (point))
  (let* ((section (magit-current-section))
         (pr (code-review-db-get-pullreq))
         (obj (oref section value))
         (map-rev (-map
                   (lambda (it)
                     `(,(cdr it) . ,(car it)))
                   code-review-reaction-types))
         (reaction-text (get-text-property (point) 'emojify-text))
         (gh-value
          (downcase (alist-get reaction-text map-rev nil nil 'equal))))
    (code-review--toggle-reaction-at-point
     pr
     (oref obj context-name)
     (oref obj comment-id)
     (oref obj reactions)
     gh-value)))

;;; * Diff Section
;;;; * Overwrite advice in magit wash diff
;; The package rationale so far was to overwrite the `magit-diff-wash-hunk` to
;; sneak in comment sections while magit-diff renders the diff. The main idea
;; was trying to use the main iterations to render the buffer to also include
;; comments thus avoiding traversing the whole buffer again.
;;
;; So far this decision delivers an usable product, however we might continue to
;; have performance problems in the future. Alternative ideas are very
;; welcoming.

(defun code-review-render--magit-diff-insert-file-section
    (file orig status modes rename header &optional long-status)
  "Overwrite the original Magit function on `magit-diff.el' FILE.
ORIG, STATUS, MODES, RENAME, HEADER and LONG-STATUS are arguments of the original fn."

  ;;; --- beg -- code-review specific code.
  ;;; I need to set a reference point for the first hunk header
  ;;; so the positioning of comments is done correctly.
  (let* ((raw-path-name (substring-no-properties file))
         (clean-path (if (string-prefix-p "b/" raw-path-name)
                         (replace-regexp-in-string "^b\\/" "" raw-path-name)
                       raw-path-name)))
    (code-review-db--curr-path-update clean-path))
    ;;; --- end -- code-review specific code.

  (magit-insert-section section
    (file file (or (equal status "deleted")
                   (derived-mode-p 'magit-status-mode)))
    (insert (propertize (format "%-10s %s" status
                                (if (or (not orig) (equal orig file))
                                    file
                                  (format "%s -> %s" orig file)))
                        'font-lock-face 'magit-diff-file-heading))
    (when long-status
      (insert (format " (%s)" long-status)))
    (magit-insert-heading)
    (unless (equal orig file)
      (oset section source orig))
    (oset section header header)
    (when modes
      (magit-insert-section (hunk '(chmod))
        (insert modes)
        (magit-insert-heading)))
    (when rename
      (magit-insert-section (hunk '(rename))
        (insert rename)
        (magit-insert-heading)))
    (when (string-match-p "Binary files.*" header)
      (magit-insert-section (code-review-binary-file-section file)
        (insert (propertize "Visit file" 'face 'code-review-request-review-face) "\n")))
    (magit-wash-sequence #'magit-diff-wash-hunk)))

(defun code-review-render--magit-diff-wash-hunk ()
  "Overwrite the original Magit function on `magit-diff.el' file.
Code Review inserts PR comments sections in the diff buffer.
Argument GROUPED-COMMENTS comments grouped by path and diff position."
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")

    ;;; --- beg -- code-review specific code.
    ;;; I need to set a reference point for the first hunk header
    ;;; so the positioning of comments is done correctly.
    (let* ((path (code-review-db--curr-path))
           (path-name (oref path name))
           (head-pos (oref path head-pos)))
      (when (not head-pos)
        (let ((adjusted-pos (+ 1 (code-review--line-number-at-pos))))
          (code-review-db--curr-path-head-pos-update path-name adjusted-pos)
          (setq head-pos adjusted-pos)
          (setq path-name path-name)))

      (when (not head-pos)
        (code-review-utils--log
         "code-review-render--magit-diff-wash-hunk"
         (format "Every diff is associated with a PATH (the file). Head pos nil for %S"
                 (prin1-to-string path)))
        (message "ERROR: Head position for path %s was not found.
Please Report this Bug" path-name))
    ;;; --- end -- code-review specific code.

      (let* ((heading  (match-string 0))
             (ranges   (mapcar (lambda (str)
                                 (mapcar #'string-to-number
                                         (split-string (substring str 1) ",")))
                               (split-string (match-string 1))))
             (about    (match-string 2))
             (combined (= (length ranges) 3))
             (value    (cons about ranges)))
        (magit-delete-line)
        (magit-insert-section section (hunk `((value . ,value)
                                              (path . ,path-name)
                                              (head-pos . ,head-pos)))
          (insert (propertize (concat heading "\n")
                              'font-lock-face 'magit-diff-hunk-heading))
          (magit-insert-heading)
          (while (not (or (eobp) (looking-at "^[^-+\s\\]")))
          ;;; --- beg -- code-review specific code.
          ;;; code-review specific code.
          ;;; add code comments
            (let* ((comment-written-pos
                    (or (alist-get path-name code-review-render-hold-written-comment-count nil nil 'equal) 0))
                   (diff-pos (+ 1 (- (code-review--line-number-at-pos)
                                     (or head-pos 0)
                                     comment-written-pos)))
                   (path-pos (code-review-utils--comment-key path-name diff-pos))
                   (written? (-contains-p code-review-render-hold-written-comment-ids path-pos))
                   (grouped-comment (code-review-utils--comment-get
                                     code-review-render-grouped-comments
                                     path-pos)))
              (if (and (not written?) grouped-comment)
                  (progn
                    (push path-pos code-review-render-hold-written-comment-ids)
                    (code-review-render-insert-comment
                     grouped-comment
                     comment-written-pos))
                (forward-line))))

          ;;; we can have outdated comments missing that were written in a
          ;;; version of the buffer that had more lines than now.
          ;;; call function to write the remaining comments.
          ;;; important to only consider comments for this path.
          (when-let (missing-paths (code-review-utils--missing-outdated-commments?
                                    path-name
                                    code-review-render-hold-written-comment-ids
                                    code-review-render-grouped-comments))
            (when (eobp)
              (code-review-render-insert-outdated-comment-missing
               path-name
               missing-paths
               code-review-render-grouped-comments)))

        ;;; --- end -- code-review specific code.
          (oset section end (point))
          (oset section washer 'magit-diff-paint-hunk)
          (oset section combined combined)
          (if combined
              (oset section from-ranges (butlast ranges))
            (oset section from-range (car ranges)))
          (oset section to-range (car (last ranges)))
          (oset section about about))))
    t))
;;;; * Render Buffer

(cl-defmethod code-review-render--auth-token-set? ((_github code-review-github-repo) res)
  "Check if the RES has a message for auth token not set for GITHUB."
  (string-prefix-p "Required Github token" (-first-item (a-get res 'error))))

(cl-defmethod code-review-render--auth-token-set? ((_gitlab code-review-gitlab-repo) res)
  "Check if the RES has a message for auth token not set for GITLAB."
  (string-prefix-p "Required Gitlab token" (-first-item (a-get res 'error))))

(cl-defmethod code-review-render--auth-token-set? (obj res)
  "Default catch all unknown values passed to this function as OBJ and RES."
  (code-review--log
   "code-review--auth-token-set?"
   (string-join (list
                 (prin1-to-string obj)
                 (prin1-to-string res))
                " <->"))
  (error "Unknown backend obj created.  Look at `code-review-log-file' and report the bug upstream"))

(cl-defmethod code-review-render--internal-build ((_github code-review-github-repo) progress res &optional buff-name msg)
  "Helper function to build process for GITHUB based on the fetched RES informing PROGRESS."
  (let* ((raw-infos (a-get-in (-second-item res) (list 'data 'repository 'pullRequest))))
    ;; 2. save raw diff data
    (progress-reporter-update progress 3)
    (code-review-db--pullreq-raw-diff-update
     (code-review-utils--clean-diff-prefixes
      (a-get (-first-item res) 'message)))

    ;; 2.1 save raw info data e.g. data from GraphQL API
    (progress-reporter-update progress 4)
    (code-review-db--pullreq-raw-infos-update raw-infos)

    ;; 2.2 trigger renders
    (progress-reporter-update progress 5)))

(cl-defmethod code-review-render--internal-build ((_gitlab code-review-gitlab-repo) progress res &optional buff-name msg)
  "Helper function to build process for GITLAB based on the fetched RES informing PROGRESS."
  ;; 1. save raw diff data
  (progress-reporter-update progress 3)
  (code-review-db--pullreq-raw-diff-update
   (code-review-gitlab-fix-diff
    (a-get (-first-item res) 'changes)))

  ;; 1.1. compute position line numbers to diff line numbers
  (progress-reporter-update progress 4)
  (code-review-gitlab-pos-line-number->diff-line-number
   (a-get (-first-item res) 'changes))

  ;; 1.2. save raw info data e.g. data from GraphQL API
  (progress-reporter-update progress 5)
  (code-review-db--pullreq-raw-infos-update
   (code-review-gitlab-fix-infos
    (a-get-in (-second-item res) (list 'data 'repository 'pullRequest))))

  ;; 1.3. trigger renders
  (progress-reporter-update progress 6))

(defun code-review-render--trigger-hooks (buff-name &optional commit-focus? msg)
  "Trigger magit section hooks and draw BUFF-NAME.
Run code review commit buffer hook when COMMIT-FOCUS? is non-nil.
If you want to display a minibuffer MSG in the end."
  (unwind-protect
      (progn
        ;; advices
        (advice-add 'magit-diff-insert-file-section :override #'code-review-render--magit-diff-insert-file-section)
        (advice-add 'magit-diff-wash-hunk :override #'code-review-render--magit-diff-wash-hunk)

        (setq code-review-render-grouped-comments
              (code-review-utils-make-group
               (code-review-db--pullreq-raw-comments))
              code-review-render-hold-written-comment-count nil
              code-review-render-hold-written-comment-ids nil)

        (with-current-buffer (get-buffer-create buff-name)
          (let* ((window (get-buffer-window buff-name))
                 (ws (window-start window))
                 (inhibit-read-only t))
            (save-excursion
              (erase-buffer)
              (insert (code-review-db--pullreq-raw-diff))
              (insert ?\n))
            (magit-insert-section (review-buffer)
              (magit-insert-section (code-review)
                (if commit-focus?
                    (magit-run-section-hook 'code-review-renders-commit-hook)
                  (magit-run-section-hook 'code-review-renders-hook)))
              (magit-wash-sequence
               (apply-partially #'magit-diff-wash-diff ())))
            (if window
                (progn
                  (pop-to-buffer buff-name)
                  (set-window-start window ws)
                  (when code-review-comment-cursor-pos
                    (goto-char code-review-comment-cursor-pos)))
              (progn
                (funcall code-review-new-buffer-window-strategy buff-name)
                (goto-char (point-min))))
            (if commit-focus?
                (progn
                  (code-review-mode)
                  (code-review-commit-minor-mode))
              (code-review-mode))
            (code-review-render--header-title)
            (when msg
              (message nil)
              (message msg)))))

    ;; remove advices
    (advice-remove 'magit-diff-insert-file-section #'code-review-render--magit-diff-insert-file-section)
    (advice-remove 'magit-diff-wash-hunk #'code-review-render--magit-diff-wash-hunk)))

(defun code-review-render--build-buffer (buff-name &optional full-refresh? commit-focus? msg)
  "Build BUFF-NAME, FULL-REFRESH? to force reload, COMMIT-FOCUS? commit hooks.
If you want to provide a MSG for the end of the process."
  (if (not full-refresh?)
      (code-review--trigger-hooks buff-name commit-focus? msg)
    (let ((obj (code-review-db-get-pullreq))
          (progress (make-progress-reporter "Fetch diff PR..." 1 6)))
      (progress-reporter-update progress 1)
      (deferred:$
        (deferred:parallel
          (lambda () (code-review-diff-deferred obj))
          (lambda () (code-review-infos-deferred obj)))
        (deferred:nextc it
          (lambda (x)
            (progress-reporter-update progress 2)
            (if (code-review-render--auth-token-set? obj x)
                (progn
                  (progress-reporter-done progress)
                  (message "Required %s token. Look at the README for how to setup your Personal Access Token"
                           (cond
                            ((code-review-github-repo-p obj)
                             "Github")
                            ((code-review-gitlab-repo-p obj)
                             "Gitlab")
                            (t "Unknown"))))
              (code-review-render--internal-build obj progress x buff-name msg)
              (code-review-render--trigger-hooks buff-name msg)
              (progress-reporter-done progress))))
        (deferred:error it
          (lambda (err)
            (code-review-utils--log
             "code-review-render--build-buffer"
             (prin1-to-string err))
            (if (and (sequencep err) (string-prefix-p "BUG: Unknown extended header:" (-second-item err)))
                (message "Your PR might have diffs too large. Currently not supported.")
              (message "Got an error from your VC provider. Check `code-review-log-file'."))))))))
;;;; * Comments

(defgeneric code-review-comment-insert-lines (obj)
  "Insert comment lines in the code section based on section type denoted by OBJ.")

(cl-defmethod code-review-comment-insert-lines ((obj code-review-local-comment-section))
  "Insert local comment lines present in the OBJ."
  (magit-insert-section (code-review-local-comment-section obj)
    (let ((heading "Comment by YOU: "))
      (add-face-text-property 0 (length heading) (oref obj heading-face) t heading)
      (magit-insert-heading heading))
    (magit-insert-section (code-review-local-comment-section obj)
      (dolist (l (code-review-utils--split-comment
                  (code-review-utils--wrap-text
                   (oref obj msg)
                   code-review-fill-column)))
        (insert l)
        (insert ?\n)))))

(cl-defmethod code-review-comment-insert-lines ((obj code-review-reply-comment-section))
  "Insert reply comment lines present in the OBJ."
  (magit-insert-section (code-review-reply-comment-section obj)
    (let ((heading "Reply by YOU: "))
      (add-face-text-property 0 (length heading) (oref obj heading-face) t heading)
      (magit-insert-heading heading))
    (magit-insert-section (code-review-reply-comment-section obj)
      (dolist (l (code-review-utils--split-comment
                  (code-review-utils--wrap-text
                   (oref obj msg)
                   code-review-fill-column)))
        (insert l)
        (insert ?\n)))))

(cl-defmethod code-review-insert-comment-lines ((obj code-review-comment-section))
  "Insert the comment lines given in the OBJ."
  (let ((body-lines (code-review-utils--split-comment (oref obj msg))))
    (dolist (l body-lines)
      (insert l)
      (insert ?\n))))

(cl-defmethod code-review-comment-insert-lines (obj)
  "Default insert comment lines in the OBJ."
  (magit-insert-section (code-review-code-comment-section obj)
    (let ((heading (concat
                    (propertize "Reviewed by " 'face 'magit-section-heading)
                    (propertize (concat "@" (oref obj author)) 'face 'code-review-author-face)
                    " - "
                    (code-review--propertize-keyword (oref obj state))
                    " - "
                    (propertize (code-review-utils--format-timestamp (oref obj createdAt)) 'face 'code-review-timestamp-face))))
      (add-face-text-property 0 (length heading) 'code-review-recent-comment-heading t heading)
      (magit-insert-heading heading))
    (magit-insert-section (code-review-code-comment-section obj)
      (dolist (l (code-review-utils--split-comment
                  (code-review-utils--wrap-text
                   (oref obj msg)
                   code-review-fill-column)))
        (insert (propertize l 'face 'code-review-comment-face))
        (insert ?\n))
      (when-let (reactions-obj (oref obj reactions))
        (code-review-comment-insert-reactions
         reactions-obj
         "code-comment"
         (oref obj id))))))

(defun code-review-render-insert-comment (comments amount-loc)
  "Insert COMMENTS to PULLREQ-ID keep the AMOUNT-LOC of comments written.
A quite good assumption: every comment in an outdated hunk will be outdated."
  (if (oref (-first-item comments) outdated?)
      (code-review-render-insert-outdated-comment
       comments
       amount-loc)
    (let ((new-amount-loc amount-loc))
      (forward-line)
      (dolist (c comments)
        (let* ((body-lines (code-review-utils--split-comment (oref c msg)))
               (amount-loc-incr-partial (+ 1 (length body-lines)))
               (amount-loc-incr (if (oref c reactions)
                                    (+ 2 amount-loc-incr-partial)
                                  amount-loc-incr-partial)))
          (setq new-amount-loc (+ new-amount-loc amount-loc-incr))
          (oset c amount-loc new-amount-loc)

          (setq code-review-render-hold-written-comment-count
                (code-review-utils--comment-update-written-count
                 code-review-render-hold-written-comment-count
                 (oref c path)
                 amount-loc-incr))
          (code-review-comment-insert-lines c))))))
;;;; * Comments outdated

(defun code-review-render-insert-outdated-comment (comments amount-loc)
  "Insert outdated COMMENTS in the buffer of PULLREQ-ID considering AMOUNT-LOC."
  ;;; hunk groups are necessary because we usually have multiple reviews about
  ;;; the same original position across different commits snapshots.
  ;;; as github UI we will add those hunks and its comments
  (let* ((hunk-groups (-group-by (lambda (el) (oref el diffHunk)) comments))
         (hunks (a-keys hunk-groups)))
    (dolist (hunk hunks)
      (when (not hunk)
        (code-review-utils--log
         "code-review-render-insert-outdated-comment"
         (format "Every outdated comment must have a hunk! Error found for %S"
                 (prin1-to-string hunk)))
        (message "Hunk empty found. A empty string will be used instead. Report this bug please."))
      (let* ((safe-hunk (or hunk ""))
             (diff-hunk-lines (split-string safe-hunk "\n"))
             (amount-new-loc (+ 1 (length diff-hunk-lines)))
             (first-hunk-commit (-first-item (alist-get safe-hunk hunk-groups nil nil 'equal)))
             (metadata1 `((comment . ,first-hunk-commit)
                          (amount-loc ., (+ amount-loc amount-new-loc)))))

        (setq code-review-render-hold-written-comment-count
              (code-review-utils--comment-update-written-count
               code-review-render-hold-written-comment-count
               (oref first-hunk-commit path)
               amount-new-loc))

        (magit-insert-section outdated-section (code-review-outdated-hunk-section metadata1)
          (let ((heading (format "Reviewed - [OUTDATED]")))
            (add-face-text-property 0 (length heading)
                                    'code-review-outdated-comment-heading
                                    t heading)
            (magit-insert-heading heading)
            (magit-insert-section ()
              (save-excursion
                (insert safe-hunk))
              (magit-diff-wash-hunk)
              (insert ?\n)
              (insert ?\n)

              (oset outdated-section hidden t)

              (dolist (c (alist-get safe-hunk hunk-groups nil nil 'equal))
                (let* ((body-lines (code-review-utils--split-comment
                                    (code-review-utils--wrap-text
                                     (oref c msg)
                                     code-review-fill-column)))
                       (amount-new-loc-outdated-partial (+ 2 (length body-lines)))
                       (amount-new-loc-outdated (if (oref c reactions)
                                                    (+ 2 amount-new-loc-outdated-partial)
                                                  amount-new-loc-outdated-partial)))

                  (setq code-review-render-hold-written-comment-count
                        (code-review-utils--comment-update-written-count
                         code-review-render-hold-written-comment-count
                         (oref first-hunk-commit path)
                         amount-new-loc-outdated))

                  (oset c amount-loc (+ amount-loc amount-new-loc amount-new-loc-outdated))

                  (magit-insert-section (code-review-outdated-comment-section c)
                    (magit-insert-heading (format "Reviewed by %s[%s]:"
                                                  (oref c author)
                                                  (oref c state)))
                    (magit-insert-section (code-review-outdated-comment-section c)
                      (dolist (l body-lines)
                        (insert l)
                        (insert ?\n))
                      (when-let (reactions-obj (oref c reactions))
                        (code-review-comment-insert-reactions
                         reactions-obj
                         "outdated-comment"
                         (oref c id)))))
                  (insert ?\n))))))))))

(defun code-review-render-insert-outdated-comment-missing (path-name missing-paths grouped-comments)
  "Write missing outdated comments in the end of the current path.
We need PATH-NAME, MISSING-PATHS, and GROUPED-COMMENTS to make this work."
  (dolist (path-pos missing-paths)
    (let ((comment-written-pos
           (or (alist-get path-name code-review-render-hold-written-comment-count nil nil 'equal)
               0)))
      (code-review-render-insert-outdated-comment
       (code-review-utils--comment-get
        grouped-comments
        path-pos)
       comment-written-pos)
      (push path-pos code-review-render-hold-written-comment-ids))))
;;;; * Reactions

(defun code-review-code-comment-reaction-at-point ()
  "Toggle reaction in code-comment section."
  (interactive)
  (let* ((section (magit-current-section))
         (comment-id (oref (oref section value) id)))
    (setq code-review-comment-cursor-pos (point))
    (code-review-toggle-reaction-at-point comment-id "code-comment")))

(defun code-review-code-comment--add-or-delete-reaction (comment-id reaction-id content &optional delete?)
  "Add or Delete REACTION-ID in COMMENT-ID given a CONTENT.
Optionally DELETE? flag must be set if you want to remove it."
  (let* ((pr (code-review-db-get-pullreq))
         (infos (oref pr raw-infos)))
    (let-alist infos
      (let ((new-reviews
             (-map
              (lambda (r)
                (let ((new-comments
                       (-map
                        (lambda (c)
                          (let-alist c
                            (when (equal .databaseId comment-id)
                              (let ((reactions-nodes
                                     (if delete?
                                         (-filter (lambda (it)
                                                    (not (string-equal (a-get it 'id) reaction-id)))
                                                  .reactions.nodes)
                                       (append .reactions.nodes
                                               (list (a-alist 'id reaction-id 'content (upcase content)))))))
                                (setf (alist-get 'reactions c) (a-alist 'nodes reactions-nodes)))))
                          c)
                        (a-get-in r (list 'comments 'nodes)))))
                  (setf (alist-get 'comments r) (a-alist 'nodes new-comments))
                  r))
              .reviews.nodes)))
        (setf (alist-get 'reviews infos) (a-alist 'nodes new-reviews))
        (oset pr raw-infos infos)
        (oset pr raw-comments new-reviews)
        (code-review-db-update pr)))))

(defun code-review-code-comment-add-reaction (comment-id reaction-id content)
  "Add REACTION-ID with CONTENT in PR COMMENT-ID."
  (code-review-code-comment--add-or-delete-reaction comment-id reaction-id content))

(defun code-review-code-comment-delete-reaction (comment-id reaction-id)
  "Delete REACTION-ID for COMMENT-ID."
  (code-review-code-comment--add-or-delete-reaction comment-id reaction-id nil t))

(defun code-review-comment-insert-reactions (reactions context-name comment-id)
  "Insert REACTIONS in CONTEXT-NAME identified by COMMENT-ID."
  (let* ((reactions-obj (code-review-reactions-section
                         :comment-id comment-id
                         :reactions reactions
                         :context-name context-name)))
    (magit-insert-section (code-review-reactions-section reactions-obj)
      (let ((reactions-group (-group-by #'identity reactions)))
        (dolist (r (a-keys reactions-group))
          (let ((rit (alist-get r reactions-group nil nil 'equal)))
            (insert (alist-get (oref (-first-item rit) content)
                               code-review-reaction-types
                               nil nil 'equal))
            (insert (format " %S " (length rit)))))
        (insert ?\n)
        (insert ?\n)))))

;;; * Commit Buffer Render

(defun code-review-render--build-commit-buffer (buff-name)
  "Build commit buffer review given by BUFF-NAME."
  (code-review-render--build-buffer buff-name nil t))

;;;###autoload
(defun code-review-render-delete-comment ()
  "Delete a local comment."
  (interactive)
  (let ((buff-name (if code-review-comment-commit-buffer?
                       code-review-commit-buffer-name
                     code-review-buffer-name)))
    (with-slots (value start end) (magit-current-section)
      (code-review-db-delete-raw-comment (oref value internalId))
      (code-review-render--build-buffer buff-name))))

(provide 'code-review-render)
;;; code-review-render.el ends here
