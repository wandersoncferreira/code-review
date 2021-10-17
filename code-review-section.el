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
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'magit-utils)
(require 'magit-section)
(require 'magit-diff)
(require 'code-review-github)

(defvar code-review-section-first-hunk-header-pos nil
  "Hold the first hunk header position.
For internal usage only.")

(defvar code-review-section-grouped-comments nil
  "Hold the grouped comments info.
Used by the overwritten version of `magit-diff-wash-hunk'.
For internal usage only.")

(defun code-review-section-diff-pos ()
  "Compute the true diff position by discounting additional lines in the buffer."
  (let ((curr-pos (line-number-at-pos)))
    (- curr-pos code-review-section-first-hunk-header-pos)))

(defun add-comment-here? (grouped-comments)
  "Verify if a comment should be added at point based on GROUPED-COMMENTS."
  (a-get grouped-comments (code-review-section-diff-pos)))

(defun code-review-section-insert-comment (grouped-comments)
  "Insert GROUPED-COMMENTS in the buffer."
  (dolist (c (a-get grouped-comments (code-review-section-diff-pos)))
    (let ((body-lines (split-string (a-get c 'bodyText) "\n")))
      (magit-insert-section (comment c)
        (insert (format "Reviewed by @%s[%s]:"
                        (a-get c 'author)
                        (a-get c 'state)))
        (put-text-property
         (line-beginning-position)
         (1+ (line-end-position))
         'font-lock-face
         'magit-diff-hunk-heading)
        (magit-insert-heading)
        (magit-insert-section (comment c)
          (dolist (l body-lines)
            (insert l)
            (insert "\n")))))))

(defun magit-diff-wash-hunk ()
  "Overwrite the original Magit function on `magit-diff.el' file.
Code Review inserts PR comments sections in the diff buffer."
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")

    ;;; code-review specific code.
    ;;; I need to set a reference point for the first hunk header
    ;;; so the positioning of comments is done correctly.
    (when (not code-review-section-first-hunk-header-pos)
      (setq code-review-section-first-hunk-header-pos
            (+ 1 (line-number-at-pos))))

    (let* ((heading  (match-string 0))
           (ranges   (mapcar (lambda (str)
                               (mapcar #'string-to-number
                                       (split-string (substring str 1) ",")))
                             (split-string (match-string 1))))
           (about    (match-string 2))
           (combined (= (length ranges) 3))
           (value    (cons about ranges)))
      (magit-delete-line)
      (magit-insert-section section (hunk value)
        (insert (propertize (concat heading "\n")
                            'font-lock-face 'magit-diff-hunk-heading))
        (magit-insert-heading)
        (while (not (or (eobp) (looking-at "^[^-+\s\\]")))
          ;;; code-review specific code.
          ;;; add code comments
          (if (add-comment-here? code-review-section-grouped-comments)
              (code-review-section-insert-comment code-review-section-grouped-comments)
            (forward-line)))
        (oset section end (point))
        (oset section washer 'magit-diff-paint-hunk)
        (oset section combined combined)
        (if combined
            (oset section from-ranges (butlast ranges))
          (oset section from-range (car ranges)))
        (oset section to-range (car (last ranges)))
        (oset section about about)))
    t))

(defun code-review-section-insert-headers (pull-request)
  "Insert header with PULL-REQUEST data."
  (let-alist pull-request
    (magit-insert-section (_)
      (insert (format "%-11s" "Title: ") .title)
      (magit-insert-heading)
      (magit-insert-section (_)
        (insert (format "%-11s" "State: ") (or (format "%s\n" .state) "none\n")))
      (magit-insert-section (_)
        (insert (format "%-11s" "Refs: ") "tbd\n"))
      (magit-insert-section (_)
        (insert (format "%-11s" "Milestone: ") "tbd\n"))
      (magit-insert-section (_)
        (insert (format "%-11s" "Labels: ") "tbd\n"))
      (magit-insert-section (_)
        (insert (format "%-11s" "Assignees: ") "tbd\n"))
      (magit-insert-section (_)
        (insert (format "%-11s" "Review-Requests: ") "tbd\n"))))
  (insert ?\n))

(defun code-review-section-insert-commits ()
  "Insert commits section."
  (magit-insert-section (commits)
    (insert "Commits")
    (magit-insert-heading))
  (insert ?\n))

(defun code-review-section-wash (pull-request grouped-comments)
  "Format buffer text with PULL-REQUEST and GROUPED-COMMENTS info."

  ;;; unfortunately, this data needs to be passed to a magit function
  ;;; deep in the call stack.
  (setq code-review-section-grouped-comments grouped-comments)

  (let-alist pull-request
    (magit-insert-section (_)
      (setq header-line-format
            (concat (propertize " " 'display '(space :align-to 0))
                    (format "#%s: %s" .number .title)))
      (code-review-section-insert-headers pull-request)
      (code-review-section-insert-commits)
      (magit-diff-wash-diff ()))))

(provide 'code-review-section)
;;; code-review-section.el ends here
