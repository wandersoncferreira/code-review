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

(defvar comment-cursor-pos nil
  "Variable to hold the cursor position where the comment will be added.
For internal usage only.")

(defvar comment-feedback? nil
  "Differentiate between a regular comment from the main feedback comment.
For internal usage only.")

(defvar comment-editing? nil
  "Are you editing a comment box?
For internal usage only.")

(defvar comment-metadata nil
  "Metadata to be attached to the comment section.
For internal usage only.")

(defvar comment-window-configuration nil
  "Hold window configuration when adding comments.
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
              ((string-equal type "comment")
               `((reply? . t)
                 (database-id . ,.databaseId)
                 (position ,(or .position .originalPosition))))
              ((-contains-p '(local-comment
                              local-comment-header)
                            type)
               (a-assoc value 'start start 'end end))
              (t
               (progn
                 (message "You can only comment on HUNK or COMMENTS.")
                 nil)))))
        (setq comment-metadata metadata)))))


;;; Public APIs

;;;###autoload
(defun code-review-comment-add ()
  "Add comment."
  (interactive)
  (code-review-comment--hold-metadata)
  (when comment-metadata
    (let ((buffer (get-buffer-create code-review-comment-buffer-name)))
      (with-current-buffer buffer
        (insert code-review-comment-buffer-msg)
        (insert ?\n))
      (setq comment-cursor-pos (line-beginning-position))
      (setq comment-window-configuration (current-window-configuration))
      (switch-to-buffer-other-window buffer)
      (code-review-comment-mode))))

;;;###autoload
(defun code-review-comment-add-feedback ()
  "Add review FEEDBACK."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name)))
    (with-current-buffer buffer
      (insert code-review-comment-feedback-msg)
      (insert ?\n))
    (switch-to-buffer-other-window buffer)
    (setq comment-feedback? t)
    (code-review-comment-mode)))

;;;###autoload
(defun code-review-comment-commit ()
  "Commit comment."
  (interactive)
  (let* ((buffer (get-buffer code-review-comment-buffer-name))
         (comment-text (with-current-buffer buffer
                         (save-excursion
                           (buffer-substring-no-properties (point-min) (point-max))))))
    (if comment-feedback?
        (let ((comment-cleaned
               (code-review-utils--comment-clean-msg
                comment-text
                code-review-comment-feedback-msg)))
          (code-review-section-insert-feedback comment-cleaned)
          (setq comment-feedback? nil))
      (let* ((comment-cleaned (code-review-utils--comment-clean-msg
                               comment-text
                               code-review-comment-buffer-msg))
             (metadata (a-assoc comment-metadata
                                'body comment-cleaned
                                'cursor-pos comment-cursor-pos
                                'editing? comment-editing?)))
        (code-review-section-insert-local-comment comment-cleaned metadata)
        (setq comment-metadata nil
              comment-cursor-pos nil
              comment-editing? nil)))
    (set-window-configuration comment-window-configuration)
    (setq comment-window-configuration nil)))

;;;###autoload
(defun code-review-comment-edit ()
  "Add comment."
  (interactive)
  (let ((section (magit-current-section)))
    (if section
        (with-slots (type value start end) section
          (let-alist value
            (if (-contains-p '(local-comment
                               local-comment-header
                               feedback
                               feedback-header)
                             type)
                (progn
                  (code-review-comment--hold-metadata)
                  (let ((buffer (get-buffer-create code-review-comment-buffer-name)))
                    (with-current-buffer buffer
                      (insert .body)
                      (insert ?\n)
                      (goto-char (point-min)))
                    (setq comment-cursor-pos (line-beginning-position))
                    (setq comment-window-configuration (current-window-configuration))
                    (setq comment-editing? t)
                    (switch-to-buffer-other-window buffer)
                    (code-review-comment-mode)))
              (message "Edit not supported in this section"))))
      (message "You should call me on a section"))))

;;;###autoload
(defun code-review-comment-delete ()
  "Add comment."
  (interactive)
  (code-review-section-delete-local-comment))

(setq code-review-comment-mode-map
  (let ((map (copy-keymap markdown-mode-map)))
    (define-key map (kbd "C-c C-c") 'code-review-comment-commit)
    (set-keymap-parent map markdown-mode-map)
    map))

(define-derived-mode code-review-comment-mode markdown-mode "Code Review Comment"
  "Code Review Comment")

(provide 'code-review-comment)
;;; code-review-comment.el ends here
