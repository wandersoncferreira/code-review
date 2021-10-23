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

(require 'a)
(require 'dash)
(require 'magit-git)

;;;
(defvar code-review-buffer-name)

;;; COMMENTS

(defun code-review-utils--comment-key (path pos)
  "Define a key using PATH and POS."
  (format "%s:%s" path pos))

(defun code-review-utils--comment-get (grouped-comments path-pos)
  "Get comments from GROUPED-COMMENTS located by PATH-POS key."
  (alist-get path-pos grouped-comments nil nil 'equal))

(defun code-review-utils--comment-update-written-count (count-comments path comments)
  "Update how many comment lines was written for a given PATH.
COUNT-COMMENTS keep track of this value and compute line numbers
using COMMENTS."
  (let ((len(+ 1 (length comments))))
    (if-let (count (alist-get path count-comments nil nil 'equal))
        (a-assoc count-comments path (+ count len))
      (a-assoc count-comments path len))))

(defun code-review-utils--comment-clean-msg (msg text-to-remove)
  "Remove TEXT-TO-REMOVE from MSG."
  (string-trim
   (replace-regexp-in-string
    (concat (concat text-to-remove "\n") "\\|" text-to-remove)
    ""
    msg)))

(defun code-review-utils--clean-suggestion (suggestion)
  "Clean SUGGESTION comment."
  (let ((res (-reduce-from
              (lambda (acc line)
                (let ((str (string-trim line)))
                  (cond
                   ((string-empty-p str)
                    acc)

                   ((string-match-p "Suggested change" str)
                    (a-update acc 'block (lambda (v)
                                           (cons str v))))

                   (t
                    (if (not (a-get acc 'negative))
                        (-> acc
                            (a-update 'block (lambda (v)
                                               (cons (format "-   %s" str) v)))
                            (a-assoc 'negative t))
                      (-> acc
                          (a-update 'block (lambda (v)
                                             (cons (format "+   %s" str) v)))
                          (a-assoc 'negative nil)))))))
              (a-alist 'negative nil
                       'block (list))
              (split-string suggestion "\n"))))
    (nreverse (a-get res 'block))))

(defun code-review-utils--split-comment (comment)
  "Clean and split the COMMENT in lines."
  (if (string-match-p "Suggested change" comment)
      (code-review-utils--clean-suggestion comment)
    (split-string comment "\n")))

;;; GIT

(defun code-review-utils--git-get-user ()
  "Get user from forge or from user profile as fallback."
  (or (magit-get "github.user")
      (magit-get "user.name")))

;;; URL PARSE

(defun code-review-utils-pr-from-url (url)
  "Extract a pr alist from a pull request URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)" url)
         (a-alist 'num   (match-string 3 url)
                  'repo  (match-string 2 url)
                  'owner (match-string 1 url)
                  'url url))))

(defun code-review-utils-build-obj (pr-alist)
  "Return obj from PR-ALIST."
  (let-alist  pr-alist
    (cond
     ((string-match "github" .url)
      (code-review-db--pullreq-create
       (code-review-github-repo :owner .owner
                                :repo .repo
                                :number .num)))
     (t
      (message "Forge not supported")))))

(defun code-review-utils-build-obj-from-url (url)
  "Return obj from URL."
  (let ((pr-alist (code-review-utils-pr-from-url url)))
    (code-review-utils-build-obj pr-alist)))


;;; COLORS

;; Borrowed from `forge-topic.el' -- Handling label overlay colors
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


;;; SECTION

(defun code-review-utils--gen-submit-structure ()
  "Return A-LIST with replies and reviews to submit."
  (let* ((replies nil)
         (review-comments nil)
         (body nil)
         (pullreq (code-review-db-get-pullreq))
         (obj (code-review-github-repo :owner (oref pullreq owner)
                                       :repo (oref pullreq repo)
                                       :number (oref pullreq number))))
    (with-current-buffer (get-buffer code-review-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (magit-wash-sequence
         (lambda ()
           (magit-insert-section (_)
             (with-slots (type value) (magit-current-section)
               (when (string-equal type "local-comment")
                 (let-alist value
                   (if .reply?
                       (push value replies)
                     (push value review-comments)))))
             (forward-line))))))
    (let* ((partial-review `((commit_id . ,(oref pullreq sha))
                             (body . ,(oref pullreq feedback))))
           (review (if (equal nil review-comments)
                       partial-review
                     (a-assoc partial-review
                              'comments review-comments))))
      (oset obj replies replies)
      (oset obj review review)
      obj)))

(provide 'code-review-utils)
;;; code-review-utils.el ends here
