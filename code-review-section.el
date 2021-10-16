;;; code-review-section.el --- Helpers for the UI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/wandersoncferreira/code-review-section
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
(require 'code-review-github)
(require 'code-review-diff)
(require 'transient)
(require 'posframe)

(defmacro code-review-section-with-buffer (&rest body)
  "Include BODY in the buffer."
  (declare (indent 0))
  `(let ((buffer (get-buffer-create "*section demo*")))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (magit-section-mode)
         (magit-insert-section (demo-buffer)
           ,@body)))
     (switch-to-buffer-other-window buffer)))

(save-excursion
  (search-forward-regexp "@@ " nil t))

(defun code-review-section-insert-hunk (line beg end)
  "BEG and END of hunk initial LINE."
  (magit-insert-section (beg)
    (delete-region beg end)
    (insert line)
    (put-text-property
     (line-beginning-position)
     (1+ (line-end-position))
     'font-lock-face
     'magit-diff-hunk-heading)
    (magit-insert-heading)
    (let* ((beg2 (save-excursion (forward-line) (line-beginning-position)))
           (end2 (or (save-excursion
                       (when (search-forward-regexp "@@ " nil t)
                         (line-beginning-position)))
                     (buffer-end 1)))
           (hunk (buffer-substring beg2 end2)))
      (delete-region beg2 end2)
      (dolist (l (split-string (substring-no-properties hunk) "\n"))
        (insert l)
        (cond
         ((code-review-diff-added? l)
          (put-text-property
           (line-beginning-position)
           (1+ (line-end-position))
           'font-lock-face
           'magit-diff-added))
         ((code-review-diff-removed? l)
          (put-text-property
           (line-beginning-position)
           (1+ (line-end-position))
           'font-lock-face
           'magit-diff-removed))
         ((string-match-p "^local" l)
          (magit-insert-section (comment)
            (let* ((beg-l (line-beginning-position))
                   (end-l (line-end-position))
                   (comment (buffer-substring beg-l end-l)))
              (delete-region beg-l end-l)
              (magit-insert-heading "COMMENT")
              (magit-insert-section (com)
                (insert comment))))))
        (insert "\n"))
      (kill-line))))

(defun code-review-section-insert-header (line)
  "Current LINE."
  (magit-insert-section (line)
    (put-text-property
     (line-beginning-position)
     (1+ (line-end-position))
     'font-lock-face
     'diff-header)
    (forward-line)))

(defun code-review-section-insert-file-header (line)
  "Current LINE."
  (magit-insert-section (line)
    (put-text-property
     (line-beginning-position)
     (1+ (line-end-position))
     'font-lock-face
     'diff-file-header)
    (forward-line)))

(defun code-review-wash ()
  (unless (eobp)
    (let* ((beg  (line-beginning-position))
           (end (line-end-position))
           (line (buffer-substring beg end)))
      (cond
       ((code-review-diff-hunk? line)
        (code-review-section-insert-hunk line beg end))

       ((code-review-diff-header? line)
        (code-review-section-insert-header line))

       ((code-review-diff-file-header? line)
        (code-review-section-insert-file-header line))

       (t
        (magit-insert-section (demo-file)
          (forward-line)))))))

(defun code-review-section-build-buffer (pr-alist)
  "PR-ALIST."
  (deferred:$
    (deferred:parallel
      (lambda () (code-review-github-get-diff-deferred pr-alist)))
    (deferred:nextc it
      (lambda (x)
        (code-review-section-with-buffer
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

(setq segure nil)

(setq diff-pos nil)

(defun code-review-add-comment ()
  (interactive)
  (let ((buffer (get-buffer-create "*section comment*")))
    (with-current-buffer buffer
      (insert ";;; Write your comment \n"))
    (setq diff-pos (line-beginning-position))
    (switch-to-buffer-other-window buffer)))

(defun code-review-commit-comment ()
  (interactive)
  (let ((str  (with-current-buffer (get-buffer-create "*section comment*")
                (save-excursion
                  (buffer-substring-no-properties (point-min) (point-max))))))
    (kill-buffer "*section comment*")
    (with-current-buffer (get-buffer "*section demo*")
      (let ((inhibit-read-only t))
        (goto-char diff-pos)
        (insert "local @bartuka:")
        (dolist (l (split-string str "\n"))
          (when (not (string-match-p ";;; Write your comment" l))
            (insert l)
            (insert "\n")))
        (let* ((beg (point-min))
               (end (point-max))
               (whole-buffer (buffer-substring beg end)))
          (erase-buffer)
          (goto-char (point-min))
          (magit-insert-section (comments)
            (save-excursion
              (insert (substring-no-properties whole-buffer)))
            (magit-wash-sequence
             (lambda ()
               (code-review-wash)))))))))

;; (code-review-section-build-buffer
;;  '((owner . "eval-all-software")
;;    (repo . "tempo")
;;    (num . 98)))

;; (code-review-section-build-buffer
;;  '((owner . "charignon")
;;    (repo . "github-review")
;;    (num . 63)))


(provide 'code-review-section)
;;; code-review-section.el ends here
