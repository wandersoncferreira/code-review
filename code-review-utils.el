;;; code-review-utils.el --- General helpers -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.1
;; Homepage: https://github.com/wandersoncferreira/code-review
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
(require 'code-review-github)

;;;
(defvar code-review-buffer-name)
(defvar code-review-commit-buffer-name)
(defvar code-review-log-file)
(defvar code-review-comment-cursor-pos)

(declare-function code-review--build-buffer "code-review" (buffer-name &optional commit-focus? msg))

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

(defun code-review-utils--wrap-text (text n)
  "Wrap TEXT at every N column."
  (let ((fill-column n))
    (with-temp-buffer
      (insert text)
      (fill-paragraph)
      (buffer-substring (point-min) (point-max)))))

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

(defun code-review-utils-make-group (raw-comments)
  "Group RAW-COMMENTS to ease the access when building the buffer."
  (-reduce-from
   (lambda (acc node)
     (let ((author (a-get-in node (list 'author 'login)))
           (state (a-get node 'state)))
       (if-let (comments (a-get-in node (list 'comments 'nodes)))
           (-reduce-from
            (lambda (grouped-comments comment)
              (let-alist comment
                (let* ((handled-pos (or .position .originalPosition))
                       (path-pos (code-review-utils--comment-key .path handled-pos))
                       (reactions (-map
                                   (lambda (r)
                                     (code-review-reaction-section
                                      :id (a-get r 'id)
                                      :content (a-get r 'content)))
                                   .reactions.nodes))
                       (obj (cond
                             (.reply?
                              (code-review-reply-comment-section
                               :state state
                               :author author
                               :msg .bodyText
                               :position handled-pos
                               :reactions nil
                               :path .path
                               :diffHunk .diffHunk
                               :internalId .internal-id
                               :id .databaseId
                               :createdAt .createdAt
                               :updatedAt .updatedAt))
                             (.outdated
                              (code-review-outdated-comment-section
                               :state state
                               :author author
                               :msg .bodyText
                               :position handled-pos
                               :reactions reactions
                               :internalId .internal-id
                               :path .path
                               :diffHunk .diffHunk
                               :id .databaseId
                               :createdAt .createdAt
                               :updatedAt .updatedAt))
                             (.local?
                              (code-review-local-comment-section
                               :state state
                               :author author
                               :msg .bodyText
                               :position handled-pos
                               :reactions nil
                               :internalId .internal-id
                               :path .path
                               :createdAt .createdAt
                               :updatedAt .updatedAt
                               :line-type .line-type))
                             (t
                              (code-review-code-comment-section
                               :state state
                               :author author
                               :msg .bodyText
                               :position handled-pos
                               :reactions reactions
                               :internalId .internal-id
                               :path .path
                               :diffHunk .diffHunk
                               :id .databaseId
                               :createdAt .createdAt
                               :updatedAt .updatedAt)))))

                  ;;; extra checks
                  (when (not handled-pos)
                    (throw :code-review/comment-missing-position
                           "Every comment requires a position in the diff."))

                  (when (not .path)
                    (throw :code-review/comment-missing-path
                           "Every comment requires a path in the diff."))

                  (when (not .bodyText)
                    (code-review-utils--log
                     "code-review-comment-make-group"
                     (format "Every comment should have a body. Nil value found. %S"
                             (prin1-to-string comment)))
                    (message "Comment with nil body"))

                  ;;; TODO: should I guarantee that every comment has an associated diffHunk?
                  ;;; this is currently not true for local comments.
                  (if (or (not grouped-comments)
                          (not (code-review-utils--comment-get grouped-comments path-pos)))
                      (a-assoc grouped-comments path-pos (list obj))
                    (a-update grouped-comments path-pos (lambda (v) (append v (list obj))))))))
            acc
            comments)
         acc)))
   nil
   raw-comments))


;;; GIT

(defun code-review-utils--git-get-user ()
  "Get user from forge or from user profile as fallback."
  (or (magit-get "github.user")
      (magit-get "user.name")))

;;; URL PARSE

(defun code-review-utils-pr-from-url (url)
  "Extract a pr alist from a pull request URL."
  (cond
   ((string-prefix-p "https://gitlab.com" url)
    (save-match-data
      (and (string-match ".*/\\(.*\\)/\\(.*\\)/-/merge_requests/\\([0-9]+\\)" url)
           (a-alist 'num (match-string 3 url)
                    'repo (match-string 2 url)
                    'owner (match-string 1 url)
                    'forge 'gitlab
                    'url url))))
   ((string-prefix-p "https://github.com" url)
    (save-match-data
      (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)" url)
           (a-alist 'num   (match-string 3 url)
                    'repo  (match-string 2 url)
                    'owner (match-string 1 url)
                    'forge 'github
                    'url url))))))

(defun code-review-utils-build-obj (pr-alist)
  "Return obj from PR-ALIST."
  (let-alist  pr-alist
    (cond
     ((equal .forge 'github)
      (code-review-db--pullreq-create
       (code-review-github-repo :owner .owner
                                :repo .repo
                                :number .num)))
     ((equal .forge 'gitlab)
      (code-review-db--pullreq-create
       (code-review-gitlab-repo :owner .owner
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
           (let ((section (magit-current-section)))
             (with-slots (type value) section
               (cond
                ((code-review-reply-comment-section-p section))
                ((equal type 'code-review-reply-comment-header)
                 (let-alist value
                   (push `((comment-id . ,.comment.databaseId)
                           (body . ,.comment.bodyText))
                         replies)))
                ((equal type 'code-review-feedback)
                 (setq feedback (or (a-get value 'feedback) feedback)))
                ((equal type 'code-review-local-comment-header)
                 (let-alist value
                   (push `((path . ,.comment.path)
                           (position . ,.comment.position)
                           (body . ,.comment.bodyText))
                         review-comments))))
               (forward-line)))))))
    (oset pullreq replies replies)
    (oset pullreq review review-comments)
    (oset pullreq feedback feedback)
    pullreq))

;;; Forge interface

(defun code-review-utils--start-from-forge-at-point ()
  "Start from forge at point."
  (let* ((pullreq (or (forge-pullreq-at-point) (forge-current-topic)))
         (repo    (forge-get-repository pullreq))
         (number (oref pullreq number)))
    (if (not (forge-pullreq-p pullreq))
        (message "We can only review PRs at the moment. You tried on something else.")
      (let* ((pr-alist (a-alist 'owner   (oref repo owner)
                                'repo    (oref repo name)
                                'num     (cond
                                          ((numberp number)
                                           (number-to-string number))
                                          ((stringp number)
                                           number)
                                          (t
                                           (error "Pull Request has unrecognizable number value")))
                                'forge (cond
                                        ((forge-github-repository-p repo)
                                         'github)
                                        ((forge-gitlab-repository-p repo)
                                         'gitlab)
                                        (t
                                         (error "Backend not supported!"))))))
        (code-review-utils-build-obj pr-alist)
        (code-review--build-buffer
         code-review-buffer-name)))))

;;; Header setters
(defun code-review-utils--set-label-field (obj)
  "Helper function to set header multi value fields given by OP-NAME and OBJ.
Milestones, labels, projects, and more."
  (when-let (options (code-review-core-get-labels obj))
    (let* ((choices (completing-read-multiple "Choose: " options))
           (labels (append
                    (-map (lambda (x)
                            `((name . ,x)
                              (color . "0075ca")))
                          choices)
                    (oref obj labels))))
      (setq code-review-comment-cursor-pos (point))
      (oset obj labels labels)
      (code-review-core-set-labels
       obj
       (lambda ()
         (closql-insert (code-review-db) obj t)
         (code-review--build-buffer
          (code-review-utils-current-project-buffer-name)))))))

(defun code-review-utils--set-assignee-field (obj &optional assignee)
  "Helper function to set assignees header field given an OBJ.
If a valid ASSIGNEE is provided, use that instead."
  (let ((candidate nil))
    (if assignee
        (setq candidate assignee)
      (when-let (options (code-review-core-get-assignees obj))
        (let* ((choice (completing-read "Choose: " options)))
          (setq candidate choice))))
    (oset obj assignees (list `((name) (login . ,candidate))))
    (code-review-core-set-assignee
     obj
     (lambda ()
       (closql-insert (code-review-db) obj t)
       (code-review--build-buffer
        (code-review-utils-current-project-buffer-name))))))

(defun code-review-utils--set-milestone-field (obj)
  "Helper function to set a milestone given an OBJ."
  (when-let (options (code-review-core-get-milestones obj))
    (let* ((choice (completing-read "Choose: " (a-keys options)))
           (milestone `((title . ,choice)
                        (perc . 0)
                        (number .,(alist-get choice options nil nil 'equal)))))
      (setq code-review-comment-cursor-pos (point))
      (oset obj milestones milestone)
      (code-review-core-set-milestone
       obj
       (lambda ()
         (closql-insert (code-review-db) obj t)
         (code-review--build-buffer
          (code-review-utils-current-project-buffer-name)))))))

(defun code-review-utils--set-title-field (title)
  "Helper function to set a TITLE."
  (let ((pr (code-review-db-get-pullreq)))
    (setq code-review-comment-cursor-pos (point))
    (oset pr title title)
    (code-review-core-set-title
     pr
     (lambda ()
       (closql-insert (code-review-db) pr t)
       (code-review--build-buffer
        code-review-buffer-name)))))

(defun code-review-utils--set-description-field (description)
  "Helper function to set a DESCRIPTION."
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr description description)
    (code-review-core-set-description
     pr
     (lambda ()
       (closql-insert (code-review-db) pr t)
       (code-review--build-buffer
        code-review-buffer-name)))))

(defun code-review-utils--set-feedback-field (feedback)
  "Helper function to set a FEEDBACK."
  (code-review-db--pullreq-feedback-update feedback)
  (code-review--build-buffer
   code-review-buffer-name))

;;; LOG

(defun code-review-utils--log (origin msg)
  "Log MSG from ORIGIN to error file."
  (with-temp-file code-review-log-file
    (when (not (file-exists-p code-review-log-file))
      (write-file code-review-log-file))
    (insert-file-contents code-review-log-file)
    (goto-char (point-max))
    (insert ?\n)
    (insert (current-time-string))
    (insert " - ")
    (insert origin)
    (insert " - ")
    (insert msg)
    (insert ?\n)))

;;; DIFF

(defun code-review-utils--clean-diff-prefixes (raw-diff)
  "Remove all prefixes from RAW-DIFF.
Expect the same output as `git diff --no-prefix`"
  (let ((res raw-diff))
    (setq res
          (replace-regexp-in-string
           (rx line-start "diff --git a/" (group-n 1 (+? not-newline)) " b/" (backref 1) line-end)
           "diff --git \\1 \\1"
           res))
    (setq res (replace-regexp-in-string
               (rx line-start (group-n 1 (or "+++" "---")) " " (or "a/" "b/") (group-n 2 (+? not-newline)) line-end)
               "\\1 \\2"
               res))
    (string-trim res)))


;;; DATE

(defun code-review-utils--format-timestamp (str)
  "Convert and format timestamp STR from json."
  (format-time-string "%b %d, %Y, %H:%M" (date-to-time str)))

;;; line-number-at-pos replacement
;; Function taken from https://emacs.stackexchange.com/questions/3821/a-faster-method-to-obtain-line-number-at-pos-in-large-buffers
;; nlinum.el

(defvar code-review--line-number-cache nil)
(make-variable-buffer-local 'code-review--line-number-cache)

;; We could try and avoid flushing the cache at every change, e.g. with:
;;   (defun nlinum--before-change (start _end)
;;     (if (and nlinum--line-number-cache
;;              (< start (car nlinum--line-number-cache)))
;;         (save-excursion (goto-char start) (nlinum--line-number-at-pos))))
;; But it's far from clear that it's worth the trouble.  The current simplistic
;; approach seems to be good enough in practice.

(defun code-review--after-change (&rest _args)
  "Flush cache after change."
  (setq code-review--line-number-cache nil))

(defun code-review--line-number-at-pos ()
  "Like `line-number-at-pos' but sped up with a cache."
  (let ((pos
         (if (and code-review--line-number-cache
                  (> (- (point) (point-min))
                     (abs (- (point) (car code-review--line-number-cache)))))
             (funcall (if (> (point) (car code-review--line-number-cache))
                          #'+ #'-)
                      (cdr code-review--line-number-cache)
                      (count-lines (point) (car code-review--line-number-cache)))
           (line-number-at-pos))))
    (setq code-review--line-number-cache (cons (point) pos))
    pos))

(defvar code-review-utils--bin-dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defun code-review-utils--get-graphql (forge name)
  "Get Graphql content for NAME (symbol) for a given FORGE."
  (with-temp-buffer
    (insert-file-contents-literally
     (concat code-review-utils--bin-dir "graphql/" (symbol-name forge) "/" (symbol-name name) ".graphql"))
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'code-review-utils)
;;; code-review-utils.el ends here
