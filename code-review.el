;;; code-review.el --- Perform code review from Github -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/wandersoncferreira/code-review
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'magit-section)
(require 'code-review-section)

(defconst code-review-buffer-name "*Code Review*")

(defmacro code-review-with-buffer (&rest body)
  "Include BODY in the buffer."
  (declare (indent 0))
  `(let ((buffer (get-buffer-create code-review-buffer-name)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (code-review-mode)
         (magit-insert-section (review-buffer)
           ,@body)))
     (switch-to-buffer-other-window buffer)))

(defun code-review-section-build-buffer (pr-alist)
  "PR-ALIST."
  (deferred:$
    (deferred:parallel
      (lambda () (code-review-github-get-diff-deferred pr-alist)))
    (deferred:nextc it
      (lambda (x)
        (code-review-with-buffer
          (magit-insert-section (demo)
            (save-excursion
              (insert (a-get (-first-item x) 'message)))
            (magit-wash-sequence
             (lambda ()
               (code-review-wash))))
          (goto-char (point-min)))))
    (deferred:error it
      (lambda (err)
        (message "Got an error from your VC provider %S!" err)))))

(defun code-review-start ()
  (interactive)
  (code-review-section-build-buffer
   '((owner . "charignon")
     (repo . "github-review")
     (num . 63))))

;;;###autoload
(defun code-review-add-comment ()
  "Add comment."
  (interactive)
  (message " ADDED "))

;;;###autoload
(defun code-review-edit-comment ()
  "Add comment."
  (interactive)
  (message " EDIT "))

;;;###autoload
(defun code-review-delete-comment ()
  "Add comment."
  (interactive)
  (message " DELETED "))

;;; transient
(define-transient-command code-review-transient-api ()
  "Code Review"
  ["Comments"
   ("a" "Add" code-review-add-comment)
   ("e" "Edit" code-review-edit-comment)
   ("d" "Delete" code-review-delete-comment)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

;; TODO change setq to defvar
(setq code-review-mode-map
      (let ((map (copy-keymap magit-section-mode-map)))
        (suppress-keymap map t)
        (define-key map (kbd "c") 'code-review-transient-api)
        (set-keymap-parent map magit-section-mode-map)
        map))

(define-derived-mode code-review-mode magit-section-mode "Code Review"
  "Code Review mode")

(provide 'code-review)
;;; code-review.el ends here
