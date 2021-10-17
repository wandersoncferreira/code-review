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
  "A-LIST to hold the first hunk header position for each path.
For internal usage only.")

(defvar code-review-section-written-comments-count nil
  "A-LIST to hold how many lines of comments written for each path.
For internal usage only.")

(defvar code-review-section-written-comments-ident nil
  "LIST to hold the identifiers comments written.
For internal usage only.")

(defvar code-review-section-grouped-comments nil
  "Hold the grouped comments info.
Used by the overwritten version of `magit-diff-wash-hunk'.
For internal usage only.")

(defvar code-review-section-file nil
  "For internal usage only.")

(defun code-review-section-diff-pos ()
  "Compute the true diff position by discounting additional lines in the buffer."
  (let ((curr-pos (line-number-at-pos))
        (hunk-pos (or (alist-get code-review-section-file
                                 code-review-section-first-hunk-header-pos
                                 nil nil 'equal)
                      0))
        (comments-written-pos (or (alist-get code-review-section-file
                                             code-review-section-written-comments-count
                                             nil nil 'equal)
                                  0)))
    (- curr-pos
       hunk-pos
       comments-written-pos)))

(defun code-review-section-insert-outdated-comment (comments)
  "Insert outdated COMMENTS in the buffer."

  ;;; hunk groups are necessary because we usually have multiple reviews about
  ;;; the same original position accross different commits snapshots.
  ;;; as github UI we will add those hunks and its comments
  (let* ((hunk-groups (-group-by (lambda (el) (a-get el 'diffHunk)) comments))
         (hunks (a-keys hunk-groups)))
    (dolist (hunk hunks)
      (let* ((diff-hunk-lines (split-string hunk "\n"))
             (first-hunk-commit (-first-item (alist-get hunk hunk-groups nil nil 'equal))))

        (setq code-review-section-written-comments-count
              (code-review-utils-update-count-comments-written
               code-review-section-written-comments-count
               code-review-section-file
               (+ 1 (length diff-hunk-lines))))

        (magit-insert-section (comment first-hunk-commit)
          (insert (format "Reviewed by %s[%s]: [OUTDATED]"
                          (a-get first-hunk-commit 'author)
                          (a-get first-hunk-commit 'state)))
          (put-text-property
           (line-beginning-position)
           (1+ (line-end-position))
           'font-lock-face
           'magit-diff-hunk-heading)
          (magit-insert-heading)
          (magit-insert-section (hunk hunk)
            (dolist (l diff-hunk-lines)
              (insert l)
              (insert "\n"))

            (dolist (c (alist-get hunk hunk-groups nil nil 'equal))
              (let ((body-lines (split-string (a-get c 'bodyText) "\n")))

                (setq code-review-section-written-comments-count
                      (code-review-utils-update-count-comments-written
                       code-review-section-written-comments-count
                       code-review-section-file
                       (+ 1 (length body-lines))))

                (magit-insert-section (comment c)
                  (insert (format "Reviewed by %s[%s]:"
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
                      (insert "\n"))))))))))))

(defun code-review-section-insert-comment (comments)
  "Insert COMMENTS in the buffer.
A quite good assumption: every comment in an outdated hunk will be outdated."
  (if (a-get (-first-item comments) 'outdated)
      (code-review-section-insert-outdated-comment comments)
    (dolist (c comments)
      (let ((body-lines (split-string (a-get c 'bodyText) "\n")))

        (setq code-review-section-written-comments-count
              (code-review-utils-update-count-comments-written
               code-review-section-written-comments-count
               code-review-section-file
               (+ 1 (length body-lines))))

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
              (insert "\n"))))))))

(defun magit-diff-insert-file-section
    (file orig status modes rename header &optional long-status)
  "Overwrite the original Magit function on `magit-diff.el' file."

  ;;; code-review specific code.
  ;;; I need to set a reference point for the first hunk header
  ;;; so the positioning of comments is done correctly.
  (setq code-review-section-file (substring-no-properties file))

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

(defun magit-diff-wash-hunk ()
  "Overwrite the original Magit function on `magit-diff.el' file.
Code Review inserts PR comments sections in the diff buffer."
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")

    ;;; code-review specific code.
    ;;; I need to set a reference point for the first hunk header
    ;;; so the positioning of comments is done correctly.
    (setf code-review-section-first-hunk-header-pos
          (code-review-utils-update-first-hunk-pos
           code-review-section-first-hunk-header-pos
           code-review-section-file
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
          (let ((path-pos (code-review-utils-path-pos-key code-review-section-file (code-review-section-diff-pos))))
            (if-let (grouped-comments (and
                                       (not (code-review-utils-already-written?
                                             code-review-section-written-comments-ident
                                             path-pos))
                                       (code-review-utils-get-comments
                                        code-review-section-grouped-comments
                                        path-pos)))
                (progn
                  (add-to-list 'code-review-section-written-comments-ident path-pos)
                  (code-review-section-insert-comment grouped-comments))
              (forward-line))))
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

(defun code-review-section-wash (grouped-comments)
  "Format buffer text with PULL-REQUEST and GROUPED-COMMENTS info."

  ;;; unfortunately, this data needs to be passed to a magit function
  ;;; deep in the call stack.
  (setq code-review-section-grouped-comments grouped-comments)
  (magit-diff-wash-diff ()))

(provide 'code-review-section)
;;; code-review-section.el ends here
