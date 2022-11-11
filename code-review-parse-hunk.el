;;; code-review-parse-hunk.el --- Library to parse hunk strings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: December 19, 2021
;; Version: 0.0.7
;;
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
;;  Library to parse hunk strings
;;
;;  https://stackoverflow.com/questions/41662127/how-to-comment-on-a-specific-line-number-on-a-pr-on-github
;;
;;; Code:

(require 'dash)

(defvar code-review-parse--hunk-regex
  (rx "@@ -"
      (group-n 1 (one-or-more digit))
      (opt ","
           (group-n 2 (one-or-more digit)))
      " +"
      (group-n 3 (one-or-more digit))
      (opt","
          (group-n 4 (one-or-more digit))))
  "Regex to identify hunk sections.")

(defun code-review-parse-hunk-table (hunkstring)
  "Table with old line, new line, added/deleted line and relative pos from HUNKSTRING."
  (if (string-prefix-p "Binary files" hunkstring)
      nil
    (let* ((difflines (split-string hunkstring "\n"))
           (relative 0)
           (del-counter 0)
           (add-counter 0)
           old-start
           old-num-lines
           new-start
           new-num-lines
           new-change
           change
           res)
      (dolist (diffline difflines)
        (cond
         ((and (eq 0 relative)
               (string-match code-review-parse--hunk-regex diffline))
          (setq old-start (string-to-number (match-string 1 diffline))
                old-num-lines (string-to-number (or (match-string 2 diffline) "0"))
                new-start (string-to-number (match-string 3 diffline))
                new-num-lines (string-to-number (or (match-string 4 diffline) "0"))
                del-counter old-start
                add-counter new-start
                new-change nil))

         ((and (string-match code-review-parse--hunk-regex diffline)
               (not (eq 0 relative)))
          (setq relative (+ 1 relative))
          (setq old-start (string-to-number (match-string 1 diffline))
                old-num-lines (string-to-number (or (match-string 2 diffline) "0"))
                new-start (string-to-number (match-string 3 diffline))
                new-num-lines (string-to-number (or (match-string 4 diffline) "0"))
                del-counter old-start
                add-counter new-start
                new-change nil))

         ((string-prefix-p "-" diffline)
          (setq relative (+ 1 relative))
          (setq change `((type . "del")
                         (del . t)
                         (ln . ,del-counter)
                         (relative . ,relative))
                old-num-lines (1- old-num-lines)
                del-counter (1+ del-counter)
                new-change t))

         ((string-prefix-p "+" diffline)
          (setq relative (+ 1 relative))
          (setq change `((type . "add")
                         (add . t)
                         (ln . ,add-counter)
                         (relative . ,relative))
                new-num-lines (1- new-num-lines)
                add-counter (1+ add-counter)
                new-change t))

         ((or (string-match-p "\s+" diffline)
              (string-empty-p diffline))
          (setq relative (+ 1 relative))
          (setq change `((type . "normal")
                         (normal . t)
                         (ln1 . ,del-counter)
                         (ln2 . ,add-counter)
                         (relative . ,relative))
                new-num-lines (1- new-num-lines)
                add-counter (1+ add-counter)
                del-counter (1+ del-counter)
                new-change t)))
        (when new-change
          (if (not res)
              (setq res (list change))
            (setq res (append res (list change))))))
      res)))

(defun code-review-parse-hunk-relative-pos (hunktable line-obj)
  "Given a HUNKTABLE (`code-review-parse-hunk-table') and LINE-OBJ return absolute diff pos."
  (let ((row
         (let-alist line-obj
           (if .old
               (-filter
                (lambda (it)
                  (cond
                   ((string-equal (a-get it 'type) "del")
                    (>= (a-get it 'ln) .line-pos))
                   ((string-equal (a-get it 'type) "normal")
                    (>= (a-get it 'ln1) .line-pos))))
                hunktable)
             (-filter
              (lambda (it)
                (cond
                 ((string-equal (a-get it 'type) "add")
                  (>= (a-get it 'ln) .line-pos))
                 ((string-equal (a-get it 'type) "normal")
                  (>= (a-get it 'ln2) .line-pos))))
              hunktable)))))
    (a-get (-first-item row) 'relative)))

(defun code-review-parse-hunk-line-pos (hunktable hunk-obj)
  "Given a HUNKTABLE (`code-review-parse-hunk-table') and HUNK-OBJ return line pos."
  (let-alist hunk-obj
    (let ((row (-first-item
                (-filter
                 (lambda (it)
                   (eq (a-get it 'relative) .line-pos))
                 hunktable))))
      (cond
       (.added
        (a-get row 'ln))
       (.deleted
        (a-get row 'ln))
       (.normal
        `((old-line . ,(a-get row 'ln1))
          (new-line . ,(a-get row 'ln2))))))))

(provide 'code-review-parse-hunk)
;;; code-review-parse-hunk.el ends here
