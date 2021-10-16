;;; code-review-diff.el --- Helpers for diff buffer -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/wandersoncferreira/code-review-diff
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(defun code-review-diff-hunk? (line)
  "Current LINE in the buffer."
  (string-prefix-p "@@" line))

(defun code-review-diff-added? (line)
  "Current LINE in the buffer."
  (string-prefix-p "+ " line))

(defun code-review-diff-removed? (line)
  "Current LINE in the buffer."
  (string-prefix-p "- " line))

(defun code-review-diff-header? (line)
  "Current LINE in the buffer."
  (or (string-prefix-p "diff --git" line)
      (string-prefix-p "index" line)))

(defun code-review-diff-file-header? (line)
  "Current LINE in the buffer."
  (or (string-prefix-p "--- " line)
      (string-prefix-p "+++ " line)))


(provide 'code-review-diff)
;;; code-review-diff.el ends here
