;;; code-review-comment.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 17, 2021
;; Modified: October 17, 2021
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/wandersoncferreira/code-review-comments
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'code-review-section)

;;;###autoload
(define-minor-mode code-review-comment-mode
  "Few keybindings to help you handle comments in Code Review."
  :lighter " code-review-comment"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'code-review-comment-commit)
            map))

(defconst code-review-comment-buffer-name "*Code Review Comment*")

(defconst code-review-comment-buffer-msg ";;; Type C-c C-c to include your comment")

(defvar code-review-comment-hold-cursor-pos nil
  "Variable to hold the cursor position where the comment will be added.
For internal usage only.")

;;;###autoload
(defun code-review-comment-add ()
  "Add comment."
  (interactive)
  (let ((buffer (get-buffer-create code-review-comment-buffer-name)))
    (with-current-buffer buffer
      (insert code-review-comment-buffer-msg)
      (insert ?\n))
    (setq code-review-comment-hold-cursor-pos (line-beginning-position))
    (switch-to-buffer-other-window buffer)
    (code-review-comment-mode)))

;;;###autoload
(defun code-review-comment-commit ()
  "Commit comment."
  (interactive)
  (let* ((buffer (get-buffer code-review-comment-buffer-name))
         (comment (with-current-buffer buffer
                    (save-excursion
                      (buffer-substring-no-properties (point-min) (point-max))))))
    (kill-buffer buffer)
    (with-current-buffer (get-buffer "*Code Review*")
      (let ((inhibit-read-only t))
        (goto-char code-review-comment-hold-cursor-pos)
        (forward-line)
        (insert "local @bartuka TBD: Magit help!")
        (insert ?\n)
        (dolist (l (split-string comment "\n"))
          (when (not (string-match-p code-review-comment-buffer-msg l))
            (insert l)
            (insert "\n")))))
    (other-window 1)))


;;;###autoload
(defun code-review-comment-edit ()
  "Add comment."
  (interactive)
  (message " EDIT "))

;;;###autoload
(defun code-review-comment-delete ()
  "Add comment."
  (interactive)
  (message " DELETED "))


(provide 'code-review-comment)
;;; code-review-comment.el ends here
