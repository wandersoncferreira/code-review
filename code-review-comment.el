;;; code-review-comment.el --- Manage comments -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.1
;; Homepage: https://github.com/wandersoncferreira/code-review
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

(defcustom code-review-comment-buffer-msg ";;; Type C-c C-c to include your comment"
  "Helper text on top of comment buffer."
  :group 'code-review
  :type 'string)

(defcustom code-review-comment-feedback-msg ";;; Leave a comment here."
  "Default text to feedback slot."
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

(defvar code-review-comment-uncommitted nil
  "Hold uncommitted comment.
For internal usage only.")

(defvar code-review-comment-commit-buffer? nil
  "Toggle if we are at the commit review buffer.
For internal usage only.")

;; remove free variable warnings
(defvar code-review-comment-buffer-name)
(defvar code-review-commit-buffer-name)
(defvar code-review-buffer-name)

(declare-function code-review--build-buffer "code-review" (buffer-name &optional commit-focus? msg))

;;; general functons

(defun code-review-comment-reset-global-vars ()
  "Reset all stateful vars."
  (setq code-review-comment-cursor-pos nil
        code-review-comment-feedback? nil
        code-review-comment-description? nil
        code-review-comment-title? nil
        code-review-comment-commit-buffer? nil))

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

;;; Add comments

;;;###autoload
(defun code-review-comment-set-feedback ()
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
(defun code-review-comment-set-title ()
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
(defun code-review-comment-set-description ()
  "Add review description."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name))
        (pr (code-review-db-get-pullreq)))
    (setq code-review-comment-description? t)
    (setq code-review-comment-cursor-pos (point-min))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (oref pr description))
      (insert ?\n)
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

(cl-defmethod code-review-comment-handler-add-or-edit ((obj code-review-promote-comment-to-issue))
  "Edit msg and title before promoting OBJ comment to new issue."
  (setq code-review-comment-uncommitted obj
        code-review-promote-comment-to-issue? t)
  (code-review-comment-add
   (format "Title: %s\n\nBody:\n> %s"
           (oref obj title)
           (oref obj body))))

(cl-defmethod code-review-comment-handler-add-or-edit (obj)
  "Add a comment in the OBJ."
  ;;; only hunks allowed here
  (with-slots (type) (magit-current-section)
    (if (not (equal type 'hunk))
        (message "You can't add text over unspecified region.")
      (let ((current-line (line-number-at-pos))
            (amount-loc nil))
        (while (and (not (looking-at "Comment by\\|Reviewed by\\|modified\\|new file\\|deleted"))
                    (not (equal (point) (point-min))))
          (forward-line -1))
        (let ((section (magit-current-section)))
          (if (not section)
              (setq amount-loc 0)
            (with-slots (type value) section
              (if (equal type 'file)
                  (setq amount-loc 0)
                (setq amount-loc (or (oref value amount-loc) 0)))))

          (let* ((diff-pos (+ 1 (- current-line
                                   amount-loc
                                   (a-get obj 'head-pos))))
                 (local-comment (code-review-local-comment-section
                                 :state "LOCAL COMMENT"
                                 :author (code-review-utils--git-get-user)
                                 :path (a-get obj 'path)
                                 :position diff-pos)))
            (setq code-review-comment-uncommitted local-comment)
            (code-review-comment-add)))))))

;;;###autoload
(defun code-review-comment-add-or-edit ()
  "Add or edit comment depending on context."
  (interactive)
  (let ((section (magit-current-section)))
    (with-slots (value) section
      (if (code-review-reactions-section-p section)
          (code-review-reactions-reaction-at-point)
        (progn
          (setq code-review-comment-cursor-pos (point))
          (code-review-comment-handler-add-or-edit value))))))


;;; handlers COMMIT

(cl-defmethod code-review-comment-handler-commit ((obj code-review-reply-comment-section))
  "Commit the reply OBJ."
  (let* ((reply-pos (- (+ (oref obj position)
                          (length (split-string (oref obj msg) "\n")))
                       2))
         (buff-name (if code-review-comment-commit-buffer?
                        code-review-commit-buffer-name
                      code-review-buffer-name))
         (clean-msg (code-review-utils--comment-clean-msg
                     (oref obj msg)
                     code-review-comment-buffer-msg))
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
    (code-review--build-buffer buff-name)
    (setq code-review-comment-uncommitted nil)))

(cl-defmethod code-review-comment-handler-commit ((obj code-review-local-comment-section))
  "Commit the reply OBJ."
  (let* ((buff-name (if code-review-comment-commit-buffer?
                        code-review-commit-buffer-name
                      code-review-buffer-name))
         (clean-msg (code-review-utils--comment-clean-msg
                     (oref obj msg)
                     code-review-comment-buffer-msg))
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
                                          (local? . t)))))))

    (when (oref obj edit?)
      ;;; delete old comment from raw
      (code-review-db-delete-raw-comment (oref obj internalId)))

    (code-review-db--pullreq-raw-comments-update raw-comment)
    (code-review--build-buffer buff-name)
    (setq code-review-comment-uncommitted nil)))

(cl-defmethod code-review-comment-handler-commit ((obj code-review-promote-comment-to-issue))
  "Commit the promotion of comment OBJ to new issue."
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
                           (format "_Originally posted by %s in %s_"
                                   (oref obj author)
                                   (oref obj reference-link)))))
        (code-review-core-new-issue pr body title)))))

;;;###autoload
(defun code-review-comment-commit ()
  "Commit comment."
  (interactive)
  (unwind-protect
      (let* ((buffer (get-buffer code-review-comment-buffer-name))
             (comment-text (string-trim
                            (with-current-buffer buffer
                              (save-excursion
                                (buffer-substring-no-properties (point-min) (point-max)))))))
        (kill-buffer buffer)
        (cond
         (code-review-comment-description?
          (code-review-utils--set-description-field comment-text))
         (code-review-comment-title?
          (code-review-utils--set-title-field comment-text))
         (code-review-comment-feedback?
          (code-review-utils--set-feedback-field
           (code-review-utils--comment-clean-msg
            comment-text
            code-review-comment-feedback-msg)))
         (code-review-promote-comment-to-issue?
          (progn
            (oset code-review-comment-uncommitted buffer-text comment-text)
            (code-review-comment-handler-commit
             code-review-comment-uncommitted)))
         (t
          (progn
            (oset code-review-comment-uncommitted msg comment-text)
            (code-review-comment-handler-commit
             code-review-comment-uncommitted)))))
    (code-review-comment-reset-global-vars)))

;;; ----

;;;###autoload
(defun code-review-comment-quit ()
  "Quit the comment window."
  (interactive)
  (code-review-comment-reset-global-vars)
  (kill-buffer code-review-comment-buffer-name))

(defvar code-review-comment-mode-map
  (let ((map (copy-keymap markdown-mode-map)))
    (define-key map (kbd "C-c C-c") 'code-review-comment-commit)
    (define-key map (kbd "C-c C-k") 'code-review-comment-quit)
    (set-keymap-parent map markdown-mode-map)
    map))

(define-derived-mode code-review-comment-mode markdown-mode "Code Review Comment"
  "Code Review Comment.")

(provide 'code-review-comment)
;;; code-review-comment.el ends here
