;;; code-review-utils.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 17, 2021
;; Modified: October 17, 2021
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/wandersoncferreira/code-review-utils
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(defun code-review-utils-path-pos-key (path pos)
  "Define a key using PATH and POS."
  (format "%s:%s" path pos))

(defun code-review-utils-get-comments (grouped-comments path-pos)
  "Get comments from GROUPED-COMMENTS located by PATH-POS key."
  (alist-get path-pos grouped-comments nil nil 'equal))

(defun code-review-utils-update-first-hunk-pos (hunks path hunk-pos)
  "Update the HUNKS a-list to include new HUNK-POS in PATH."
  (if (not (alist-get path hunks nil nil 'equal))
      (a-assoc hunks path hunk-pos)
    hunks))

(defun code-review-utils-update-count-comments-written (count-comments path length)
  "Update COUNT-COMMENTS written by PATH using LENGTH."
  (if-let (count (alist-get path count-comments nil nil 'equal))
      (a-assoc count-comments path (+ count length))
    (a-assoc count-comments path length)))

(defun code-review-utils-already-written? (identifiers identifier)
  "Verify if IDENTIFIER is present in IDENTIFIERS."
  (-contains-p identifiers identifier))

(provide 'code-review-utils)
;;; code-review-utils.el ends here
