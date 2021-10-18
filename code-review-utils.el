;;; code-review-utils.el --- General helpers -*- lexical-binding: t; -*-
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

;;; Commentary:
;;
;;  General helper functions
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

(defun code-review-utils-clean-msg (msg text-to-remove)
  "Remove TEXT-TO-REMOVE from MSG."
  (replace-regexp-in-string
   (concat text-to-remove "\n")
   ""
   msg))

(defun code-review-utils-get-user ()
  "Get user from forge or from user profile as fallback."
  (or (magit-get "github.user")
      (magit-get "user.name")))

(defun code-review-utils-pr-from-url (url)
  "Extract a pr alist from a pull request URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)" url)
         (a-alist 'num   (match-string 3 url)
                  'repo  (match-string 2 url)
                  'owner (match-string 1 url)))))


;;; Borrowed from `forge-topic.el' -- Handling label overlay colors

(defun code-review-utils--sanitize-color (color)
  "Sanitize COLOR."
  (cond ((x-color-values color) color)
        ;; Discard alpha information.
        ((string-match-p "\\`#.\\{4\\}\\'" color) (substring color 0 3))
        ((string-match-p "\\`#.\\{8\\}\\'" color) (substring color 0 6))
        (t "#000000"))) ; Use fallback instead of invalid color.

(defun code-review-utils--contrast-color (color)
  "Return black or white depending on the luminance of COLOR."
  (if (> (code-review-utils--x-color-luminance color) 0.5) "black" "white"))

;; Copy of `rainbow-x-color-luminance'.
(defun code-review-utils--x-color-luminance (color)
  "Calculate the luminance of a COLOR string (e.g. \"#ffaa00\", \"blue\").
Return a value between 0 and 1."
  (let ((values (x-color-values color)))
    (code-review-utils--color-luminance (/ (nth 0 values) 256.0)
                            (/ (nth 1 values) 256.0)
                            (/ (nth 2 values) 256.0))))

;; Copy of `rainbow-color-luminance'.
;; Also see https://en.wikipedia.org/wiki/Relative_luminance.
(defun code-review-utils--color-luminance (red green blue)
  "Calculate the luminance of color composed of RED, GREEN and BLUE.
Return a value between 0 and 1."
  (/ (+ (* .2126 red) (* .7152 green) (* .0722 blue)) 256))

(provide 'code-review-utils)
;;; code-review-utils.el ends here
