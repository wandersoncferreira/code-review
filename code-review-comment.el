;;; code-review-comment.el --- manage comments -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>

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

(defvar code-review-comment-description? nil
  "Are you writing a description?.
For internal usage only.")

(defvar code-review-comment-editing? nil
  "Are you editing a comment box?
For internal usage only.")

(defvar code-review-comment-metadata nil
  "Metadata to be attached to the comment section.
For internal usage only.")

(defvar code-review-comment-window-configuration nil
  "Hold window configuration when adding comments.
For internal usage only.")

(defvar code-review-comment-commit? nil
  "Toggle if we are at the commit review buffer.
For internal usage only.")

;;; general functons

(defun code-review-comment-make-group (raw-comments)
  "Group RAW-COMMENTS to ease the access when building the buffer."
  (-reduce-from
   (lambda (acc node)
     (let ((author (a-get-in node (list 'author 'login)))
           (state (a-get node 'state)))
       (if-let (comments (a-get-in node (list 'comments 'nodes)))
           (-reduce-from
            (lambda (grouped-comments comment)
              (let-alist comment
                (let* ((comment-enriched (a-assoc comment 'author author 'state state))
                       (handled-pos (or .position .originalPosition))
                       (path-pos (code-review-utils--comment-key .path handled-pos)))
                  (if (or (not grouped-comments)
                          (not (code-review-utils--comment-get grouped-comments path-pos)))
                      (a-assoc grouped-comments path-pos (list comment-enriched))
                    (a-update grouped-comments path-pos (lambda (v) (append v (list comment-enriched))))))))
            acc
            comments)
         acc)))
   nil
   raw-comments))

(defun code-review-comment--hold-metadata ()
  "Save metadata to attach to local comment at commit phase."
  (with-slots (type value start end) (magit-current-section)
    (let-alist value
      (let ((metadata
             (cond
              ((string-equal type "hunk")
               `((reply? . nil)
                 (database-id . nil)))
              ((-contains-p '(code-review-comment
                              code-review-comment-header)
                            type)
               `((reply? . t)
                 (database-id . ,.comment.databaseId)
                 (comment-text . ,.comment.bodyText)
                 (position . ,(or .comment.position .comment.originalPosition))))
              ((-contains-p '(code-review-local-comment
                              code-review-local-comment-header)
                            type)
               (a-assoc value 'start start 'end end))
              (t
               (progn
                 (message "You can only comment on HUNK or COMMENTS.")
                 nil)))))
        (setq code-review-comment-metadata metadata)))))

(defun code-review-comment-add ()
  "Add comment."
  (code-review-comment--hold-metadata)
  (when code-review-comment-metadata
    (let ((buffer (get-buffer-create code-review-comment-buffer-name)))
      (setq code-review-comment-editing? nil)
      (with-current-buffer buffer
        (erase-buffer)
        (insert code-review-comment-buffer-msg)
        (insert ?\n)
        (switch-to-buffer-other-window buffer)
        (code-review-comment-mode)))))

(defun code-review-comment-edit ()
  "Edit comment."
  (with-slots (type value start end) (magit-current-section)
    (let-alist value
      (setq code-review-comment-metadata value
            code-review-comment-editing? t)
      (let ((buffer (get-buffer-create code-review-comment-buffer-name)))
        (with-current-buffer buffer
          (erase-buffer)
          (insert .comment.bodyText)
          (insert ?\n)
          (goto-char (point-min)))
        (switch-to-buffer-other-window buffer)
        (code-review-comment-mode)))))

;;; Public APIs

;;;###autoload
(defun code-review-comment-add-feedback ()
  "Add review FEEDBACK."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name)))
    (setq code-review-comment-window-configuration (current-window-configuration))
    (setq code-review-comment-feedback? t)
    (setq code-review-comment-cursor-pos (point))
    (with-current-buffer buffer
      (erase-buffer)
      (insert code-review-comment-feedback-msg)
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

;;;###autoload
(defun code-review-comment-add-title ()
  "Add review title."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name))
        (pr (code-review-db-get-pullreq)))
    (setq code-review-comment-window-configuration (current-window-configuration))
    (setq code-review-comment-title? t)
    (setq code-review-comment-cursor-pos (point))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (oref pr title))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

;;;###autoload
(defun code-review-comment-add-description ()
  "Add review description."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name))
        (pr (code-review-db-get-pullreq)))
    (setq code-review-comment-window-configuration (current-window-configuration))
    (setq code-review-comment-description? t)
    (setq code-review-comment-cursor-pos (point))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (oref pr description))
      (insert ?\n)
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

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
        (setq code-review-full-refresh? nil)
        (kill-buffer buffer)
        (cond
         (code-review-comment-description?
          (code-review-utils--set-description-field comment-text))

         (code-review-comment-title?
          (code-review-utils--set-title-field comment-text))

         (code-review-comment-feedback?
          (let* ((comment-cleaned
                  (code-review-utils--comment-clean-msg
                   comment-text
                   code-review-comment-feedback-msg)))
            (code-review-utils--set-feedback-field comment-cleaned)))

         (t
          (let* ((comment-cleaned (code-review-utils--comment-clean-msg
                                   comment-text
                                   code-review-comment-buffer-msg))
                 (buff
                  (code-review-utils--set-local-comment
                   comment-cleaned
                   (-> code-review-comment-metadata
                       (a-assoc 'cursor-pos code-review-comment-cursor-pos 'editing? code-review-comment-editing?)
                       (a-assoc-in (list 'comment 'bodyText) comment-cleaned)))))
            ;; (set-window-configuration code-review-comment-window-configuration)
            ;; (goto-char code-review-comment-cursor-pos)
            ))))
    (setq code-review-comment-metadata nil
          code-review-comment-cursor-pos nil
          code-review-comment-editing? nil
          code-review-comment-feedback? nil
          code-review-comment-description? nil
          code-review-comment-title? nil
          code-review-comment-window-configuration nil
          code-review-comment-commit? nil)))

;;;###autoload
(defun code-review-comment-quit ()
  "Quit the comment window."
  (interactive)
  (setq code-review-comment-metadata nil
        code-review-comment-cursor-pos nil
        code-review-comment-window-configuration nil
        code-review-comment-feedback? nil
        code-review-comment-editing? nil))

;;;###autoload
(defun code-review-comment-add-or-edit ()
  "Add or edit comment depending on context."
  (interactive)
  (setq code-review-comment-commit? (string-equal (buffer-name (current-buffer)) code-review-commit-buffer-name)
        code-review-comment-cursor-pos (line-beginning-position)
        code-review-comment-window-configuration (current-window-configuration))

  (let ((section (magit-current-section)))
    (if (not section)
        (message "You should call on a section.")
      (with-slots (type value) section
        (cond
         ((-contains-p '(code-review-local-comment
                         code-review-local-comment-header
                         code-review-reply-comment
                         code-review-reply-comment-header)
                       type)
          (code-review-comment-edit))
         ((string-equal type "hunk")
          (code-review-comment-add))
         ((-contains-p '(code-review-comment-body
                         code-review-comment-header)
                       type)
          (code-review-comment-add))
         ((-contains-p '(code-review-feedback-header
                         code-review-feedback)
                       type)
          (if (not (a-get value 'feedback))
              (code-review-comment-add-feedback)
            (progn
              (setq code-review-comment-feedback? t)
              (code-review-comment-edit))))
         (t
          (message "Invalid operation")))))))

;;;###autoload
(defun code-review-comment-delete ()
  "Delete comment."
  (interactive)
  (code-review-section-delete-local-comment))

(defvar code-review-comment-mode-map
  (let ((map (copy-keymap markdown-mode-map)))
    (define-key map (kbd "C-c C-c") 'code-review-comment-commit)
    (define-key map (kbd "C-c C-k") 'code-review-comment-quit)
    (set-keymap-parent map markdown-mode-map)
    map))

(define-derived-mode code-review-comment-mode markdown-mode "Code Review Comment"
  "Code Review Comment")

(provide 'code-review-comment)
;;; code-review-comment.el ends here
