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
(require 'forge-pullreq)
(require 'forge-post)
(require 'forge-core)
(require 'forge-github)

;;;
(defvar code-review-buffer-name)
(defvar code-review-commit-buffer-name)
(defvar code-review-log-file)
(defvar code-review-comment-commit?)

(defun code-review-utils-current-project-buffer-name ()
  "Return the name of the buffer we are currently in."
  (interactive)
  (let ((name (buffer-name (current-buffer))))
    (if (-contains-p (list code-review-buffer-name
                           code-review-commit-buffer-name)
                     name)
        name
      (throw :invalid-usage "You are trying to call this function from an unexpected place."))))

;;; COMMENTS

(defun code-review-utils--comment-key (path pos)
  "Define a key using PATH and POS."
  (format "%s:%s" path pos))

(defun code-review-utils--comment-get (grouped-comments path-pos)
  "Get comments from GROUPED-COMMENTS located by PATH-POS key."
  (alist-get path-pos grouped-comments nil nil 'equal))

(defun code-review-utils--comment-update-written-count (count-comments path amount-loc-incr)
  "Update how many comment lines was written for a given PATH.
COUNT-COMMENTS keep track of this value and compute line numbers
using COMMENTS."
  (if-let (count (alist-get path count-comments nil nil 'equal))
      (a-assoc count-comments path (+ count amount-loc-incr))
    (a-assoc count-comments path amount-loc-incr)))

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
  (let ((safe-comment (or comment "")))
    (if (string-match-p "Suggested change" safe-comment)
        (code-review-utils--clean-suggestion safe-comment)
      (split-string safe-comment "\n"))))


(defun code-review-utils--missing-outdated-commments? (path-name path-pos-written grouped-comments)
  "PATH-NAME, PATH-POS-WRITTEN, and GROUPED-COMMENTS to find comments to write."
  (let* ((written-in-path (-filter
                           (lambda (ckey)
                             (string-prefix-p path-name ckey))
                           path-pos-written))
         (expected-in-path (-filter
                            (lambda (ckey)
                              (string-prefix-p path-name ckey))
                            (a-keys grouped-comments))))
    (-difference expected-in-path written-in-path)))


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

(defun code-review-utils--gen-submit-structure (&optional feedback)
  "Return A-LIST with replies and reviews to submit.
If you already have a FEEDBACK string to submit use it."
  (interactive)
  (let* ((replies nil)
         (review-comments nil)
         (feedback feedback)
         (pullreq (code-review-db-get-pullreq)))
    (with-current-buffer (get-buffer code-review-buffer-name)
      (save-excursion
        (goto-char (point-min))
        (magit-wash-sequence
         (lambda ()
           (with-slots (type value) (magit-current-section)
             (cond
              ((equal type 'code-review-reply-comment)
               (let-alist value
                 (push `((comment-id . ,.comment.bodyText)
                         (body . ,.comment.bodyText))
                       replies)))
              ((equal type 'code-review-feedback)
               (setq feedback (or (a-get value 'feedback) feedback)))
              ((equal type 'code-review-local-comment)
               (let-alist value
                 (push `((path . ,.comment.path)
                         (position . ,.comment.position)
                         (body . ,.comment.bodyText))
                       review-comments))))
             (forward-line))))
        (oset pullreq replies replies)
        (oset pullreq review review-comments)
        (oset pullreq feedback feedback)
        pullreq))))

;;; Forge interface

(defun code-review-utils--start-from-forge-at-point ()
  "Start from forge at point."
  (let* ((pullreq (or (forge-pullreq-at-point) (forge-current-topic)))
         (repo    (forge-get-repository pullreq)))

    (if (not (forge-pullreq-p pullreq))
        (message "We can only review PRs at the moment. You tried on something else.")
      (let* ((pr-alist (a-alist 'owner   (oref repo owner)
                                'repo    (oref repo name)
                                'num     (oref pullreq number)
                                'url (when (forge-github-repository-p repo)
                                       "https://api.github.com"))))
        (code-review-utils-build-obj pr-alist)
        (code-review-section--build-buffer
         code-review-buffer-name)))))

;;; Header setters

(defun code-review-utils--set-label-field (obj)
  "Helper function to set header multi value fields given by OP-NAME and OBJ.
Milestones, labels, projects, and more."
  (let* ((options (code-review-get-labels obj))
         (choices (completing-read-multiple "Choose: " options)))
    (oset obj labels choices)
    (code-review-set-labels obj)
    (closql-insert (code-review-db) obj t)
    (code-review-section--build-buffer
     (code-review-utils-current-project-buffer-name)
     t)))

(defun code-review-utils--set-assignee-field (obj &optional assignee)
  "Helper function to set assignees header field given an OBJ.
If a valid ASSIGNEE is provided, use that instead."
  (let ((candidate nil))
    (if assignee
        (setq candidate assignee)
      (let* ((options (code-review-get-assignees obj))
             (choice (completing-read "Choose: " options)))
        (setq candidate choice)))
    (oset obj assignees candidate)
    (code-review-set-assignee obj)
    (closql-insert (code-review-db) obj t)
    (code-review-section--build-buffer
     (code-review-utils-current-project-buffer-name)
     t)))

(defun code-review-utils--set-milestone-field (obj)
  "Helper function to set a milestone given an OBJ."
  (let* ((options (code-review-get-milestones obj))
         (choice (completing-read "Choose: " (a-keys options))))
    (oset obj milestones (alist-get choice options nil nil 'equal))
    (code-review-set-milestone obj)
    (closql-insert (code-review-db) obj t)
    (code-review-section--build-buffer
     (code-review-utils-current-project-buffer-name)
     t)))

(defun code-review-utils--set-title-field (title)
  "Helper function to set a TITLE."
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr title title)
    (code-review-set-title pr)
    (closql-insert (code-review-db) pr t)
    (code-review-section--build-buffer
     code-review-buffer-name
     t)))

(defun code-review-utils--set-description-field (description)
  "Helper function to set a DESCRIPTION."
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr description description)
    (code-review-set-description pr)
    (closql-insert (code-review-db) pr t)
    (code-review-section--build-buffer
     code-review-buffer-name
     t)))

(defun code-review-utils--set-feedback-field (feedback)
  "Helper function to set a FEEDBACK."
  (code-review-db--pullreq-feedback-update feedback)
  (code-review-section--build-buffer
   code-review-buffer-name
   t))


(defun code-review-utils--set-local-comment (comment metadata)
  "Insert local COMMENT based on METADATA structure."
  (let ((buff-name (if code-review-comment-commit?
                       code-review-commit-buffer-name
                     code-review-buffer-name)))
    (code-review-section-insert-local-comment
     comment
     metadata
     buff-name)
    (code-review-section--build-buffer buff-name)))

;;; LOG

(defun code-review-utils--log (origin msg)
  "Log MSG from ORIGIN to error file."
  (with-temp-buffer
    (when (not (file-exists-p code-review-log-file))
      (write-file code-review-log-file))
    (insert-file code-review-log-file)
    (goto-char (point-max))
    (insert ?\n)
    (insert (current-time-string))
    (insert " - ")
    (insert origin)
    (insert " - ")
    (insert msg)
    (insert ?\n)))

(provide 'code-review-utils)
;;; code-review-utils.el ends here
