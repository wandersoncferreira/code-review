;;; code-review-comment.el --- Manage comments -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.6
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
;; This file is not part of GNU Emacs.
;;
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
;;  This library implements support functions to manage code review comments.
;;
;;; Code:

(require 'code-review-section)
(require 'code-review-utils)
(require 'markdown-mode)

(defcustom code-review-comment-buffer-name "*Code Review Comment*"
  "Name of comment buffer."
  :group 'code-review
  :type 'string)

(defcustom code-review-comment-buffer-msg ";;; Type C-c C-c to include your comment locally\n;;; After that check the transient menu (press r) to see possible actions"
  "Helper text on top of comment buffer."
  :group 'code-review
  :type 'string)

(defcustom code-review-comment-feedback-msg ";;; Leave a comment here."
  "Default text to feedback slot."
  :group 'code-review
  :type 'string)

(defcustom code-review-comment-single-comment-msg ";;; Equivalent to add a simple comment to the PR without a review."
  "Default text to single comment section e.g. conversation."
  :group 'code-review
  :type 'string)

(defcustom code-review-comment-suggestion-msg ";;; Type C-c C-c to include your comment locally\n;;; Type C-c C-k to abort"
  "Default text to suggestion code in comment buffer."
  :group 'code-review
  :type 'string)

;;; internal vars

(defvar code-review-comment-cursor-pos nil
  "Variable to hold the cursor position where the comment will be added.
For internal usage only.")

(defvar code-review-comment-feedback? nil
  "Differentiate between a regular comment from the main feedback comment.
For internal usage only.")

(defvar code-review-comment-title? nil
  "Are you writing a title?.
For internal usage only.")

(defvar code-review-promote-comment-to-issue? nil
  "Are you promoting a comment to a new issue?.
For internal usage only.")

(defvar code-review-comment-description? nil
  "Are you writing a description?.
For internal usage only.")

(defvar code-review-comment-single-comment? nil
  "Include a single new comment to the PR without a Review.
For internal usage only.")

(defvar code-review-comment-uncommitted nil
  "Hold uncommitted comment.
For internal usage only.")

(defvar code-review-comment-commit-buffer? nil
  "Toggle if we are at the commit review buffer.
For internal usage only.")

(defvar code-review-comment-send? nil
  "Toggle if we should send the local comment immediately.
For internal usage only.")

(defvar code-review-comment-suggestion? nil
  "Are you writing a code suggestion?.
For internal usage only.")

;; remove free variable warnings
(defvar code-review-comment-buffer-name)
(defvar code-review-commit-buffer-name)
(defvar code-review-buffer-name)

;;; general functons

(defun code-review-comment-reset-global-vars ()
  "Reset all stateful vars."
  (setq code-review-comment-cursor-pos nil
        code-review-comment-feedback? nil
        code-review-comment-title? nil
        code-review-promote-comment-to-issue? nil
        code-review-comment-description? nil
        code-review-comment-uncommitted nil
        code-review-comment-commit-buffer? nil
        code-review-comment-single-comment? nil
        code-review-comment-send? nil
        code-review-comment-suggestion? nil))

;;; Comment C_UD

(defun code-review-comment-add (&optional msg)
  "Add comment.
Optionally define a MSG."
  (let ((buffer (get-buffer-create code-review-comment-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (if msg msg code-review-comment-buffer-msg))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

(defun code-review-comment-edit ()
  "Edit comment."
  (with-slots (value) (magit-current-section)
    (let ((buffer (get-buffer-create code-review-comment-buffer-name)))
      (with-current-buffer buffer
        (erase-buffer)
        (insert (oref value msg))
        (insert ?\n)
        (goto-char (point-min)))
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

;;; handlers ADD or EDIT

(cl-defmethod code-review-comment-handler-add-or-edit ((obj code-review-code-comment-section))
  "Reply to comment OBJ."
  (let ((reply-comment (code-review-reply-comment-section
                        :state (oref obj state)
                        :author (oref obj author)
                        :path (oref obj path)
                        :position (oref obj position)
                        :diffHunk (oref obj diffHunk)
                        :id (oref obj id))))
    (setq code-review-comment-uncommitted reply-comment)
    (code-review-comment-add)))

(cl-defmethod code-review-comment-handler-add-or-edit ((obj code-review-local-comment-section))
  "Edit local comment in OBJ."
  (oset obj edit? t)
  (setq code-review-comment-uncommitted obj)
  (code-review-comment-edit))

(cl-defmethod code-review-comment-handler-add-or-edit ((obj code-review-title-section))
  "Change title in OBJ."
  (setq code-review-comment-uncommitted obj
        code-review-comment-title? t)
  (code-review-comment-add))

(defclass code-review-comment-promote-to-issue ()
  ((reference-link :initarg :reference-link)
   (author :initarg :author)
   (title :initarg :title)
   (body :initarg :body)
   (buffer-text :initform nil)))

(cl-defmethod code-review-comment-handler-add-or-edit ((obj code-review-comment-promote-to-issue))
  "Edit msg and title before promoting OBJ comment to new issue."
  (setq code-review-comment-uncommitted obj
        code-review-promote-comment-to-issue? t)
  (code-review-comment-add
   (format "<!-- Do not remove the Title and Body placeholders.  -->
<!-- You can add multi-line segments in the body section. -->\n\nTitle: %s\n\nBody:\n> %s"
           (oref obj title)
           (oref obj body))))

(cl-defmethod code-review-comment-handler-add-or-edit (obj)
  "Add a comment in the OBJ."
  ;;; only hunks allowed here
  (with-slots (type) (magit-current-section)
    (if (not (equal type 'hunk))
        (message "You can't add text over unspecified region.")
      (let* ((current-line (line-number-at-pos))
             (line (save-excursion
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
             (line-type (cond
                         ((string-prefix-p "-" line)
                          "REMOVED")
                         ((string-prefix-p "+" line)
                          "ADDED")
                         (t
                          "UNCHANGED")))
             (suggestion
              (format "%s\n\n```suggestion\n %s\n"
                      code-review-comment-suggestion-msg
                      (substring line 1)))
             (amount-loc nil))
        (save-excursion
          (while (and (not (looking-at
                            "Comment by\\|Reviewed by\\|Reply by\\|modified\\|new file\\|deleted"))
                      (not (equal (point) (point-min))))
            (forward-line -1))
          (let ((section (magit-current-section)))
            (if (not section)
                (setq amount-loc 0)
              (with-slots (type value) section
                (if (equal type 'file)
                    (setq amount-loc 0)
                  (setq amount-loc (or (oref value amount-loc) 0)))))))

        (let* ((diff-pos (+ 1 (- current-line
                                 amount-loc
                                 (a-get obj 'head-pos))))
               (local-comment (code-review-local-comment-section
                               :state "LOCAL COMMENT"
                               :author (code-review-utils--git-get-user)
                               :path (a-get obj 'path)
                               :position diff-pos
                               :line-type line-type
                               :send? code-review-comment-send?)))
          (setq code-review-comment-uncommitted local-comment)
          (if code-review-comment-suggestion?
              (progn
                (code-review-comment-add suggestion)
                (with-current-buffer (get-buffer code-review-comment-buffer-name)
                  (forward-line -2)))
            (code-review-comment-add)))))))


;;;###autoload
(defun code-review-comment-add-or-edit (&optional suggestion-code?)
  "Add or edit comment depending on context.
Inform if a SUGGESTION-CODE? is being proposed."
  (interactive)
  (let ((section (magit-current-section)))
    (with-current-buffer (get-buffer code-review-buffer-name)
      (setq code-review-comment-cursor-pos (point)
            code-review-comment-suggestion? suggestion-code?)
      (with-slots (value) section
        (if (code-review-reactions-section-p section)
            (code-review-reactions-reaction-at-point)
          (code-review-comment-handler-add-or-edit value))))))

;;; handlers COMMIT

(cl-defmethod code-review-comment-handler-commit ((obj code-review-reply-comment-section) default-buff-msg)
  "Commit the reply OBJ and clean the DEFAULT-BUFF-MSG from the text if any."
  (let* ((reply-pos (- (+ (oref obj position)
                          (length (split-string (oref obj msg) "\n")))
                       2))
         (clean-msg (code-review-utils--comment-clean-msg
                     (oref obj msg)
                     default-buff-msg))
         (raw-comment `((author (login . ,(oref obj author)))
                        (state . ,(oref obj state))
                        (comments (nodes ((internal-id . ,(uuidgen-4))
                                          (bodyText . ,clean-msg)
                                          (path . ,(oref obj path))
                                          (position . ,reply-pos)
                                          (databaseId . ,(oref obj id))
                                          (diffHunk . ,(oref obj diffHunk))
                                          (outdated)
                                          (local?)
                                          (reply? . t)))))))
    (code-review-db--pullreq-raw-comments-update raw-comment)
    (code-review--build-buffer)
    (setq code-review-comment-uncommitted nil)))

(cl-defmethod code-review-comment-handler-commit ((obj code-review-local-comment-section) default-buff-msg)
  "Commit the local comment OBJ and clean the DEFAULT-BUFF-MSG from the text if any."
  (let* ((buff-name (if code-review-comment-commit-buffer?
                        code-review-commit-buffer-name
                      code-review-buffer-name))
         (clean-msg (code-review-utils--comment-clean-msg
                     (oref obj msg)
                     default-buff-msg))
         (raw-comment `((author (login . ,(oref obj author)))
                        (state . ,(oref obj state))
                        (comments (nodes ((internal-id . ,(uuidgen-4))
                                          (bodyText . ,clean-msg)
                                          (path . ,(oref obj path))
                                          (position . ,(oref obj position))
                                          (databaseId)
                                          (diffHunk)
                                          (outdated)
                                          (reply?)
                                          (line-type . ,(oref obj line-type))
                                          (local? . t)))))))

    (when (oref obj edit?)
      ;;; delete old comment from raw
      (code-review-db-delete-raw-comment (oref obj internalId)))

    (if (oref obj send?)
        (progn
          (oset obj msg clean-msg)
          (code-review-new-code-comment
           (code-review-db-get-pullreq)
           obj
           (lambda (&rest _)
             (let ((code-review-section-full-refresh? t))
               (code-review--build-buffer buff-name)))))
      (progn
        (code-review-db--pullreq-raw-comments-update raw-comment)
        (code-review--build-buffer buff-name)))))

(cl-defmethod code-review-comment-handler-commit ((obj code-review-comment-promote-to-issue) _default-buff-msg)
  "Commit the promotion of comment OBJ to new issue and clean the DEFAULT-BUFF-MSG from the text if any."
  (save-match-data
    (let ((text (oref obj buffer-text))
          (regex (rx "Title:"
                     (group-n 1 (* any) "\n")
                     (one-or-more "\n")
                     "Body:"
                     (group-n 2 (zero-or-more anything))))
          (title)
          (body))
      (and (string-match regex text)
           (setq title (match-string 1 text))
           (setq body (match-string 2 text)))
      (let* ((pr (code-review-db-get-pullreq))
             (body (concat (string-trim body)
                           "\n\n"
                           (format "_Originally posted by @%s in %s_"
                                   (oref obj author)
                                   (oref obj reference-link)))))
        (setq code-review-promote-comment-to-issue? nil)
        (code-review-new-issue
         pr body title
         (lambda (&rest _) (message "New issue created.")))))))

;;;###autoload
(defun code-review-comment-commit ()
  "Commit comment."
  (interactive)
  (unwind-protect
      (let* ((buffer (get-buffer code-review-comment-buffer-name))
             (comment-text (string-trim
                            (with-current-buffer buffer
                              (save-excursion
                                (buffer-substring-no-properties (point-min) (point-max))))))
             (pr (code-review-db-get-pullreq)))

        (kill-buffer-and-window)

        (cond

         (code-review-comment-description?
          (oset pr raw-infos (-> (oref pr raw-infos)
                                 (a-assoc 'bodyText comment-text)
                                 (a-assoc 'bodyHTML nil)))
          (code-review-send-description
           pr
           (lambda ()
             (code-review-db-update pr)
             (code-review--build-buffer)
             (code-review-comment-reset-global-vars))))

         (code-review-comment-title?
          (oset pr title comment-text)
          (code-review-send-title
           pr
           (lambda ()
             (code-review-db-update pr)
             (code-review--build-buffer)
             (code-review-comment-reset-global-vars))))

         (code-review-comment-feedback?
          (let ((msg
                 (code-review-utils--comment-clean-msg
                  comment-text
                  code-review-comment-feedback-msg)))
            (code-review-db--pullreq-feedback-update msg)
            (code-review--build-buffer)
            (code-review-comment-reset-global-vars)))

         (code-review-promote-comment-to-issue?
          (progn
            (oset code-review-comment-uncommitted buffer-text comment-text)
            (code-review-comment-handler-commit
             code-review-comment-uncommitted
             code-review-comment-buffer-msg)
            (code-review-comment-reset-global-vars)))

         (code-review-comment-send?
          (progn
            (oset code-review-comment-uncommitted msg comment-text)
            (code-review-comment-handler-commit
             code-review-comment-uncommitted
             code-review-comment-single-comment-msg)
            (code-review-comment-reset-global-vars)))

         (code-review-comment-single-comment?
          (let ((msg
                 (code-review-utils--comment-clean-msg
                  comment-text
                  code-review-comment-single-comment-msg))
                (callback (lambda (&rest _)
                            (let ((code-review-section-full-refresh? t))
                              (code-review--build-buffer)
                              (code-review-comment-reset-global-vars)))))
            (code-review-new-issue-comment pr msg callback)))
         (t
          (progn
            (oset code-review-comment-uncommitted msg comment-text)
            (code-review-comment-handler-commit
             code-review-comment-uncommitted
             (if code-review-comment-suggestion?
                 code-review-comment-suggestion-msg
               code-review-comment-buffer-msg))
            (code-review-comment-reset-global-vars)))))))

;;; ----

;;;###autoload
(defun code-review-input-mention-user-at-point ()
  "Insert @USERNAME at current point to mention an user."
  (interactive)
  (let* ((pr (code-review-db-get-pullreq))
         (user (completing-read
                "Mention user: "
                (-map
                 (lambda (it)
                   (a-get it 'login))
                 (code-review-get-assignable-users pr))
                nil 'require-match)))
    (insert "@" user " ")))

;;;###autoload
(defun code-review-comment-quit ()
  "Quit the comment window."
  (interactive)
  (kill-buffer-and-window)
  (with-current-buffer (get-buffer code-review-buffer-name)
    (goto-char code-review-comment-cursor-pos)
    (code-review-comment-reset-global-vars)))

(defvar code-review-comment-mode-map
  (let ((map (copy-keymap markdown-mode-map)))
    (define-key map (kbd "C-c C-c") 'code-review-comment-commit)
    (define-key map (kbd "C-c C-k") 'code-review-comment-quit)
    (define-key map (kbd "C-c @") 'code-review-input-mention-user-at-point)
    (set-keymap-parent map markdown-mode-map)
    map))

(define-derived-mode code-review-comment-mode markdown-mode "Code Review Comment"
  "Code Review Comment.")

(provide 'code-review-comment)
;;; code-review-comment.el ends here
