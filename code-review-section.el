;;; code-review-section.el --- UI -*- lexical-binding: t; -*-
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

;;; Commentary:
;;
;;  Code to build the UI.
;;
;;; Code:

(require 'deferred)
(require 'magit-section)
(require 'magit-diff)
(require 'code-review-core)
(require 'code-review-db)
(require 'code-review-utils)
(require 'emojify)

(defface code-review-timestamp-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DimGrey"
     :slant italic)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DimGrey"
     :slant italic))
  "Face for timestamps."
  :group 'code-review)

(defface code-review-comment-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DimGrey")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightGrey"))
  "Face for comment sections."
  :group 'code-review)

(defface code-review-state-face
  '((t :inherit bold))
  "Face used for default state keywords."
  :group 'code-review)

(defface code-review-author-face
  '((t :inherit font-lock-keyword-face))
  "Face used for author names."
  :group 'code-review)

(defface code-review-error-state-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face used for error state (e.g. changes requested)."
  :group 'code-review)

(defface code-review-success-state-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face used for success state (e.g. merged)."
  :group 'code-review)

(defface code-review-info-state-face
  '((t :slant italic))
  "Face used for info (unimportant) state (e.g. resolved)."
  :group 'code-review)

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

(declare-function code-review--build-buffer "code-review" (buffer-name &optional commit-focus? msg))

(defvar code-review-section-full-refresh? nil
  "Indicate if we want to perform a complete restart.
For internal usage only.")

(defvar code-review-section-grouped-comments nil
  "Hold grouped comments to avoid computation on every hunk line.
For internal usage only.")

(defvar code-review-section-hold-written-comment-ids nil
  "List to hold written comments ids.
For internal usage only.")

(defvar code-review-section-hold-written-comment-count nil
  "List of number of lines of comments written in the buffer.
For internal usage only.")

;;; sections

(defclass code-review-is-draft-section (magit-section)
  ((draft? :initform nil
           :type (or null string))))

(defclass code-review-title-section (magit-section)
  ((keymap  :initform 'code-review-title-section-map)
   (title  :initform nil
           :type (or null string))))

(defvar code-review-title-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-set-title)
    map)
  "Keymaps for code-comment sections.")

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

(defvar code-review-milestone-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review--set-milestone)
    map)
  "Keymaps for milestone section.")

;;; Description

(defclass code-review-description-section (magit-section)
  ((keymap :initform 'code-review-description-section-map)
   (id     :initarg :id)
   (msg    :initarg :msg)
   (reactions :initarg :reactions)))

(defvar code-review-description-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'code-review-description-reaction-at-point)
    map)
  "Keymaps for description section.")

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

;;; Feedback

(defclass code-review-feedback-section (magit-section)
  ((keymap :initform 'code-review-feedback-section-map)
   (msg    :initarg :msg)))

(defvar code-review-feedback-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-set-feedback)
    map)
  "Keymaps for feedback section.")

;;; Milestone

(cl-defmethod code-review-pretty-milestone ((obj code-review-milestone-section))
  "Get the pretty version of milestone for a given OBJ."
  (cond
   ((and (oref obj title) (oref obj perc))
    (format "%s (%s%%)"
            (oref obj title)
            (oref obj perc)))
   ((oref obj title)
    (oref obj title))
   (t
    "No milestone")))

;;; Commit

(defclass code-review-commit-section (magit-section)
  ((keymap :initform 'code-review-commit-section-map)
   (sha    :initarg :sha)
   (msg    :initarg :msg)))

(defvar code-review-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-commit-at-point)
    map)
  "Keymaps for commit section.")


;;; General Comment - Conversation

(defclass code-review-comment-section (magit-section)
  ((keymap :initform 'code-review-comment-section-map)
   (author :initarg :author
           :type string)
   (msg    :initarg :msg
           :type string)
   (id     :initarg :id
           :type (or null number))
   (reactions :initarg :reactions)
   (face   :initform 'magit-log-author)))

(defvar code-review-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'code-review-conversation-reaction-at-point)
    map)
  "Keymaps for comment section.")

(defun code-review-conversation-reaction-at-point ()
  "Toggle reaction in conversation sections."
  (interactive)
  (let* ((section (magit-current-section))
         (comment-id (oref (oref section value) id)))
    (setq code-review-comment-cursor-pos (point))
    (code-review-toggle-reaction-at-point comment-id "comment")))

(defun code-review-conversation--add-or-delete-reaction (comment-id reaction-id content &optional delete?)
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

(defun code-review-conversation-add-reaction (comment-id reaction-id content)
  "Add REACTION-ID with CONTENT in PR COMMENT-ID."
  (code-review-conversation--add-or-delete-reaction comment-id reaction-id content))

(defun code-review-conversation-delete-reaction (comment-id reaction-id)
  "Delete REACTION-ID from COMMENT-ID."
  (code-review-conversation--add-or-delete-reaction comment-id reaction-id nil t))

(cl-defmethod code-review-insert-comment-lines ((obj code-review-comment-section))
  "Insert the comment lines given in the OBJ."
  (let ((body-lines (code-review-utils--split-comment (oref obj msg))))
    (dolist (l body-lines)
      (insert l)
      (insert ?\n))))

;;; Reactions

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

(defun code-review--toggle-reaction-at-point (pr context-name comment-id existing-reactions reaction)
  "Given a PR, use the CONTEXT-NAME to toggle REACTION in COMMENT-ID considering EXISTING-REACTIONS."
  (let* ((res (code-review-core-set-reaction pr context-name comment-id reaction))
         (reaction-id (a-get res 'id))
         (node-id (a-get res 'node_id))
         (existing-reaction-ids (when existing-reactions
                                  (-map (lambda (r) (oref r id)) existing-reactions))))
    (if (-contains-p existing-reaction-ids node-id)
        (progn
          (code-review-core-delete-reaction pr context-name comment-id reaction-id)
          (pcase context-name
            ("pr-description" (code-review-description-delete-reaction node-id))
            ("comment" (code-review-conversation-delete-reaction comment-id node-id))
            ("code-comment" (code-review-code-comment-delete-reaction comment-id node-id))))
      (pcase context-name
        ("pr-description"
         (code-review-description-add-reaction node-id reaction))
        ("comment"
         (code-review-conversation-add-reaction comment-id node-id reaction))
        ("code-comment"
         (code-review-code-comment-add-reaction comment-id node-id reaction))))
    (code-review--build-buffer
     code-review-buffer-name)))

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
         (gh-value (downcase (alist-get reaction-text map-rev nil nil 'equal))))
    (code-review--toggle-reaction-at-point
     pr
     (oref obj context-name)
     (oref obj comment-id)
     (oref obj reactions)
     gh-value)))

;;; Comment, Code Comment, Reply Comment

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

(defvar code-review-code-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-r") 'code-review-code-comment-reaction-at-point)
    map)
  "Keymaps for code-comment sections.")

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

(defclass code-review-local-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-local-comment-section-map)
   (local?       :initform t)
   (reply?       :initform nil)
   (edit?        :initform nil)
   (outdated?    :initform nil)
   (heading-face :initform 'code-review-recent-comment-heading)
   (body-face    :initform nil)
   (diffHunk     :initform nil)))

(defclass code-review-reply-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-reply-comment-section-map)
   (reply?       :initform t)
   (local?       :initform t)
   (edit?        :initform nil)
   (outdated?    :initform nil)
   (heading-face :initform 'code-review-recent-comment-heading)
   (body-face    :initform nil)))

(defvar code-review-local-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-section-delete-comment)
    map)
  "Keymaps for local-comment sections.")

(defvar code-review-reply-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    (define-key map (kbd "C-c C-k") 'code-review-section-delete-comment)
    map)
  "Keymaps for reply-comment sections.")

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

(defclass code-review-outdated-comment-section (code-review-base-comment-section)
  ((keymap       :initform 'code-review-outdated-comment-section-map)
   (local?       :initform t)
   (outdated?    :initform t)))

(defvar code-review-outdated-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-comment-add-or-edit)
    map)
  "Keymaps for outdated-comment sections.")

(defclass code-review-labels-section (magit-section)
  ((keymap :initform 'code-review-labels-section-map)
   (labels :initarg :labels)))

(defvar code-review-labels-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review--set-label)
    map)
  "Keymaps for code-comment sections.")

(defclass code-review-assignees-section (magit-section)
  ((keymap :initform 'code-review-assignees-section-map)
   (assignees :initarg :assignees)))

(defvar code-review-assignees-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review--set-assignee)
    map)
  "Keymaps for code-comment sections.")

;;; headers

(defun code-review-section-insert-header-title ()
  "Insert the title header line."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (setq header-line-format
            (propertize
             (format "#%s: %s".number (code-review-db--pullreq-title))
             'font-lock-face
             'magit-section-heading)))))

;;; TODO: add some nice face to true and false
(defun code-review-section-insert-is-draft ()
  "Insert the isDraft value of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((draft? (if .isDraft "true" "false")))
        (magit-insert-section (code-review-is-draft-section draft?)
          (insert (format "%-17s" "Draft: ") draft?)
          (insert ?\n))))))

(defun code-review-section-insert-title ()
  "Insert the title of the header buffer."
  (when-let (title (code-review-db--pullreq-title))
    (magit-insert-section (code-review-title-section title)
      (insert (format "%-17s" "Title: ") title)
      (insert ?\n))))

(defun code-review-section-insert-state ()
  "Insert the state of the header buffer."
  (when-let (state (code-review-db--pullreq-state))
    (let ((value (if state state "none")))
      (magit-insert-section (code-review-state-section value)
        (insert (format "%-17s" "State: ") value)
        (insert ?\n)))))

(defun code-review-section-insert-ref ()
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

(defun code-review-section-insert-milestone ()
  "Insert the milestone of the header buffer."
  (let ((milestones (code-review-db--pullreq-milestones)))
    (let-alist milestones
      (let* ((title (when (not (string-empty-p .title)) .title))
             (obj (code-review-milestone-section :title title :perc .perc)))
        (magit-insert-section (code-review-milestone-section obj)
          (insert (format "%-17s" "Milestone: "))
          (insert (propertize (code-review-pretty-milestone obj) 'font-lock-face 'magit-dimmed))
          (insert ?\n))))))

(defun code-review-section-insert-labels ()
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
              (let* ((color (concat "#" (a-get label 'color)))
                     (background (code-review-utils--sanitize-color color))
                     (foreground (code-review-utils--contrast-color color))
                     (o (make-overlay (- (point) (length (a-get label 'name))) (point))))
                (overlay-put o 'priority 2)
                (overlay-put o 'evaporate t)
                (overlay-put o 'font-lock-face
                             `((:background ,background)
                               (:foreground ,foreground)
                               forge-topic-label)))
              (insert " "))
          (insert (propertize "None yet" 'font-lock-face 'magit-dimmed)))
        (insert ?\n)))))

(defun code-review-section-insert-assignee ()
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

(defun code-review-section-insert-project ()
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

(defun code-review-section-insert-suggested-reviewers ()
  "Insert the suggested reviewers."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((reviewers (string-join
                         (-map
                          (lambda (r)
                            (a-get-in r (list 'reviewer 'name)))
                          .suggestedReviewers)
                         ", "))
             (suggested-reviewers (if (string-empty-p reviewers)
                                      (propertize "No reviews" 'font-lock-face 'magit-dimmed)
                                    reviewers)))
        (magit-insert-section (code-review-reviewers-section suggested-reviewers)
          (insert (format "%-17s" "Suggested-Reviewers: ") suggested-reviewers)
          (insert ?\n))))))

(defun code-review-section-insert-headers ()
  "Insert all the headers."
  (magit-insert-headers 'code-review-headers-hook))

;;; next sections

(defclass code-review-check-section (magit-section)
  ((details :initarg :details)))

(defun code-review-section-insert-commits ()
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

(defun code-review-section-insert-pr-description ()
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

(defun code-review-section-insert-feedback-heading ()
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

(defun code-review-section-insert-general-comments ()
  "Insert general comments for the PULL-REQUEST in the buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review-comment-header-section)
        (insert (propertize "Conversation" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
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
              (insert ?\n))))))))

(defun code-review-section-insert-outdated-comment (comments amount-loc)
  "Insert outdated COMMENTS in the buffer of PULLREQ-ID considering AMOUNT-LOC."
  ;;; hunk groups are necessary because we usually have multiple reviews about
  ;;; the same original position across different commits snapshots.
  ;;; as github UI we will add those hunks and its comments
  (let* ((hunk-groups (-group-by (lambda (el) (oref el diffHunk)) comments))
         (hunks (a-keys hunk-groups)))
    (dolist (hunk hunks)
      (when (not hunk)
        (code-review-utils--log
         "code-review-section-insert-outdated-comment"
         (format "Every outdated comment must have a hunk! Error found for %S"
                 (prin1-to-string hunk)))
        (message "Hunk empty found. A empty string will be used instead. Report this bug please."))
      (let* ((safe-hunk (or hunk ""))
             (diff-hunk-lines (split-string safe-hunk "\n"))
             (amount-new-loc (+ 1 (length diff-hunk-lines)))
             (first-hunk-commit (-first-item (alist-get safe-hunk hunk-groups nil nil 'equal)))
             (metadata1 `((comment . ,first-hunk-commit)
                          (amount-loc ., (+ amount-loc amount-new-loc)))))

        (setq code-review-section-hold-written-comment-count
              (code-review-utils--comment-update-written-count
               code-review-section-hold-written-comment-count
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

                  (setq code-review-section-hold-written-comment-count
                        (code-review-utils--comment-update-written-count
                         code-review-section-hold-written-comment-count
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

(defun code-review-section-insert-outdated-comment-missing (path-name missing-paths grouped-comments)
  "Write missing outdated comments in the end of the current path.
We need PATH-NAME, MISSING-PATHS, and GROUPED-COMMENTS to make this work."
  (dolist (path-pos missing-paths)
    (let ((comment-written-pos
           (or (alist-get path-name code-review-section-hold-written-comment-count nil nil 'equal)
               0)))
      (code-review-section-insert-outdated-comment
       (code-review-utils--comment-get
        grouped-comments
        path-pos)
       comment-written-pos)
      (push path-pos code-review-section-hold-written-comment-ids))))

(defun code-review-section-insert-comment (comments amount-loc)
  "Insert COMMENTS to PULLREQ-ID keep the AMOUNT-LOC of comments written.
A quite good assumption: every comment in an outdated hunk will be outdated."
  (if (oref (-first-item comments) outdated?)
      (code-review-section-insert-outdated-comment
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

          (setq code-review-section-hold-written-comment-count
                (code-review-utils--comment-update-written-count
                 code-review-section-hold-written-comment-count
                 (oref c path)
                 amount-loc-incr))
          (code-review-comment-insert-lines c))))))

(defun code-review-section--magit-diff-insert-file-section
    (file orig status modes rename header &optional long-status)
  "Overwrite the original Magit function on `magit-diff.el' FILE.
ORIG, STATUS, MODES, RENAME, HEADER and LONG-STATUS are arguments of the original fn."

  (setq code-review-progress-reporter-counter
        (1+ code-review-progress-reporter-counter))
  (progress-reporter-update code-review-progress-reporter code-review-progress-reporter-counter)

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
    (magit-wash-sequence #'magit-diff-wash-hunk)))

(defun code-review-section--magit-diff-wash-hunk ()
  "Overwrite the original Magit function on `magit-diff.el' file.
Code Review inserts PR comments sections in the diff buffer.
Argument GROUPED-COMMENTS comments grouped by path and diff position."
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")

    (setq code-review-progress-reporter-counter
          (1+ code-review-progress-reporter-counter))
    (progress-reporter-update code-review-progress-reporter code-review-progress-reporter-counter)

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
         "code-review-section--magit-diff-wash-hunk"
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
            (setq code-review-progress-reporter-counter
                  (1+ code-review-progress-reporter-counter))
            (progress-reporter-update code-review-progress-reporter code-review-progress-reporter-counter)
          ;;; --- beg -- code-review specific code.
          ;;; code-review specific code.
          ;;; add code comments
            (let* ((comment-written-pos
                    (or (alist-get path-name code-review-section-hold-written-comment-count nil nil 'equal) 0))
                   (diff-pos (+ 1 (- (code-review--line-number-at-pos)
                                     (or head-pos 0)
                                     comment-written-pos)))
                   (path-pos (code-review-utils--comment-key path-name diff-pos))
                   (written? (-contains-p code-review-section-hold-written-comment-ids path-pos))
                   (grouped-comment (code-review-utils--comment-get
                                     code-review-section-grouped-comments
                                     path-pos)))
              (if (and (not written?) grouped-comment)
                  (progn
                    (push path-pos code-review-section-hold-written-comment-ids)
                    (code-review-section-insert-comment
                     grouped-comment
                     comment-written-pos))
                (forward-line))))

          ;;; we can have outdated comments missing that were written in a
          ;;; version of the buffer that had more lines than now.
          ;;; call function to write the remaining comments.
          ;;; important to only consider comments for this path.
          (when-let (missing-paths (code-review-utils--missing-outdated-commments?
                                    path-name
                                    code-review-section-hold-written-comment-ids
                                    code-review-section-grouped-comments))
            (when (eobp)
              (code-review-section-insert-outdated-comment-missing
               path-name
               missing-paths
               code-review-section-grouped-comments)))

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

(defun code-review-section--build-commit-buffer (buff-name)
  "Build commit buffer review given by BUFF-NAME."
  (code-review--build-buffer buff-name t))

;;;###autoload
(defun code-review-section-delete-comment ()
  "Delete a local comment."
  (interactive)
  (let ((buff-name (if code-review-comment-commit-buffer?
                       code-review-commit-buffer-name
                     code-review-buffer-name)))
    (with-slots (value start end) (magit-current-section)
      (code-review-db-delete-raw-comment (oref value internalId))
      (code-review--build-buffer buff-name))))

(provide 'code-review-section)
;;; code-review-section.el ends here
