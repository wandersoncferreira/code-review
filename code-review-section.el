;;; code-review-section.el --- UI -*- lexical-binding: t; -*-
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
;;  Code to build the UI.
;;
;;; Code:

(require 'magit-section)
(require 'magit-diff)
(require 'code-review-db)

(defvar code-review-section-grouped-comments nil
  "Hold the grouped comments info.
Used by the overwritten version of `magit-diff-wash-hunk'.
For internal usage only.")

(defvar code-review-section-commit-comments nil
  "Hold the grouped comments info from a commit.
Used by the overwritten version of `magit-diff-wash-hunk'.
For internal usage only.")

(defvar code-review-pullreq-info nil
  "Hold the pull-request data.
For internal usage only.")


;;; headers

(defun code-review-section-insert-header-title ()
  "Insert the title header line."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (setq header-line-format
            (propertize
             (format "#%s: %s" .number .title)
             'font-lock-face
             'magit-section-heading)))))

(defun code-review-section-insert-title ()
  "Insert the title of the header buffer."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (magit-insert-section (code-review:title .title)
        (insert (format "%-17s" "Title: ") .title)
        (insert ?\n)))))

(defun code-review-section-insert-state ()
  "Insert the state of the header buffer."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (magit-insert-section (code-review:state .state)
        (insert (format "%-17s" "State: ") (or (format "%s" .state) "none"))
        (insert ?\n)))))

(defun code-review-section-insert-ref ()
  "Insert the state of the header buffer."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (magit-insert-section (code-review:ref `((base . ,.baseRefName)
                                               (head . ,.headRefName)))
        (insert (format "%-17s" "Refs: "))
        (insert .baseRefName)
        (insert (propertize " ... " 'font-lock-face 'magit-dimmed))
        (insert .headRefName)
        (insert ?\n)))))

(defun code-review-section-insert-milestone ()
  "Insert the milestone of the header buffer."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (magit-insert-section (code-review:milestone `((title . ,.milestone.title)
                                                     (progress . ,.milestone.progressPercentage)))
        (insert (format "%-17s" "Milestone: ")
                (format "%s (%s%%)"
                        .milestone.title
                        .milestone.progressPercentage))
        (insert ?\n)))))

(defun code-review-section-insert-labels ()
  "Insert the labels of the header buffer."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (magit-insert-section (code-review:labels .labels.nodes)
        (insert (format "%-17s" "Labels: "))
        (dolist (label .labels.nodes)
          (insert (a-get label 'name))
          (let* ((color (concat "#" (a-get label 'color)))
                 (background (code-review-utils--sanitize-color color))
                 (foreground (code-review-utils--contrast-color color))
                 (o (make-overlay (- (point) (length (a-get label 'name))) (point))))
            (overlay-put o 'priority 2)
            (overlay-put o 'evaporate t)
            (overlay-put o 'font-lock-face
                         `((:background ,background)
                           (:foreground ,foreground)
                           forge-topic-label)))
          (insert " "))
        (insert ?\n)))))

(defun code-review-section-insert-assignee ()
  "Insert the assignee of the header buffer."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (let* ((assignee-names (-map
                              (lambda (a)
                                (format "%s (@%s)"
                                        (a-get a 'name)
                                        (a-get a 'login)))
                              .assignees.nodes))
             (assignees (string-join assignee-names ", ")))
        (magit-insert-section (code-review:assignee assignees)
          (insert (format "%-17s" "Assignees: ") assignees)
          (insert ?\n))))))

(defun code-review-section-insert-project ()
  "Insert the project of the header buffer."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (let* ((project-names (-map
                             (lambda (p)
                               (a-get-in p (list 'project 'name)))
                             .projectCards.nodes))
             (projects (string-join project-names ", ")))
        (magit-insert-section (code-review:project projects)
          (insert (format "%-17s" "Projects: ") projects)
          (insert ?\n))))))

(defun code-review-section-insert-suggested-reviewers ()
  "Insert the suggested reviewers."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (let* ((reviewers (string-join .suggestedReviewers ", "))
             (suggested-reviewers (if (string-empty-p reviewers)
                                      (propertize "No reviews" 'font-lock-face 'magit-dimmed)
                                    reviewers)))
        (magit-insert-section (code-review:reviewers suggested-reviewers)
          (insert (format "%-17s" "Suggested-Reviewers: ") suggested-reviewers))))))

(defun code-review-section-insert-headers ()
  "Insert all the headers."
  (magit-insert-headers 'code-review-headers-hook))

;;; next sections

(defun code-review-section-insert-commits ()
  "Insert commits from PULL-REQUEST."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (magit-insert-section (code-review:commits-header)
        (insert (propertize "Commits" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (magit-insert-section (code-review:commits-body)
          (dolist (c .commits.nodes)
            (let ((commit-value `((sha ,(a-get-in c (list 'commit 'abbreviatedOid))))))
              (magit-insert-section (code-review:commit commit-value)
                (insert (propertize
                         (format "%-6s " (a-get-in c (list 'commit 'abbreviatedOid)))
                         'font-lock-face 'magit-hash)
                        (a-get-in c (list 'commit 'message)))))
            (insert ?\n)))
        (insert ?\n)))))


(defun code-review-section-insert-pr-description ()
  "Insert PULL-REQUEST description."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (magit-insert-section (code-review:pr-description-header)
        (insert (propertize "Description" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (magit-insert-section (code-review:pr-description)
          (if (string-empty-p .bodyText)
              (insert (propertize "No description provided." 'font-lock-face 'magit-dimmed))
            (insert .bodyText))
          (insert ?\n)
          (insert ?\n))))))

(defun code-review-section-insert-feedback-heading ()
  "Insert feedback heading."
  (magit-insert-section (code-review:feedback-header)
    (insert (propertize "Your Review Feedback" 'font-lock-face 'magit-section-heading))
    (magit-insert-heading)
    (magit-insert-section (code-review:feedback)
      (insert (propertize "Leave a comment here." 'font-lock-face 'magit-dimmed))
      (insert ?\n))
    (insert ?\n)))

(defun code-review-section-insert-general-comments ()
  "Insert general comments for the PULL-REQUEST in the buffer."
  (when code-review-pullreq-info
    (let-alist code-review-pullreq-info
      (magit-insert-section (code-review:conversation-header)
        (insert (propertize "Conversation" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (dolist (c .comments.nodes)
          (magit-insert-section (code-review:general-comment c)
            (insert (propertize
                     (format "@%s" (a-get-in c (list 'author 'login)))
                     'font-lock-face
                     'magit-log-author))
            (magit-insert-heading)
            (let ((body-lines (code-review-utils--split-comment (a-get c 'bodyText))))
              (dolist (l body-lines)
                (insert l)
                (insert ?\n)))))
        (insert ?\n)))))


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

        (code-review-db--curr-path-comment-count-update
         code-review-pullreq-id
         (+ 1 (length diff-hunk-lines)))

        (magit-insert-section (comment first-hunk-commit)
          (let ((heading (format "Reviewed by %s [%s] - [OUTDATED]"
                                 (a-get first-hunk-commit 'author)
                                 (a-get first-hunk-commit 'state))))
            (add-face-text-property 0 (length heading)
                                    'code-review-outdated-comment-heading
                                    t heading)
            (magit-insert-heading heading)
            (magit-insert-section ()
              (save-excursion
                (insert hunk))
              (magit-diff-wash-hunk)
              (insert ?\n)

              (dolist (c (alist-get hunk hunk-groups nil nil 'equal))
                (let ((body-lines (code-review-utils--split-comment (a-get c 'bodyText))))
                  (code-review-db--curr-path-comment-count-update
                   code-review-pullreq-id
                   (+ 2 (length body-lines)))

                  (magit-insert-section (comment-outdated-headind c)
                    (magit-insert-heading (format "Reviewed by %s[%s]:"
                                                  (a-get c 'author)
                                                  (a-get c 'state)))
                    (magit-insert-section (comment-outdated c)
                      (dolist (l body-lines)
                        (insert l)
                        (insert ?\n))))
                  (insert ?\n))))))))))

(defun code-review-section-insert-comment (comments)
  "Insert COMMENTS in the buffer.
A quite good assumption: every comment in an outdated hunk will be outdated."
  (if (a-get (-first-item comments) 'outdated)
      (code-review-section-insert-outdated-comment comments)
    (dolist (c comments)
      (let ((body-lines (code-review-utils--split-comment (a-get c 'bodyText))))

        (code-review-db--curr-path-comment-count-update
         code-review-pullreq-id
         (+ 2 (length body-lines)))

        (magit-insert-section (comment c)
          (let ((heading (format "Reviewed by @%s [%s]: "
                                 (a-get c 'author)
                                 (a-get c 'state))))
            (add-face-text-property 0 (length heading)
                                    'code-review-recent-comment-heading t heading)
            (magit-insert-heading heading))
          (magit-insert-section (comment c)
            (dolist (l body-lines)
              (insert l)
              (insert "\n"))
            (insert ?\n)))))))

(defun code-review-section--magit-diff-insert-file-section
    (file orig status modes rename header &optional long-status)
  "Overwrite the original Magit function on `magit-diff.el' file."

  ;;; --- beg -- code-review specific code.
  ;;; I need to set a reference point for the first hunk header
  ;;; so the positioning of comments is done correctly.
  (code-review-db--curr-path-update
   code-review-pullreq-id
   (substring-no-properties file))
  ;;; --- end -- code-review specific code.

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

(defun code-review-section--magit-diff-wash-hunk ()
  "Overwrite the original Magit function on `magit-diff.el' file.
Code Review inserts PR comments sections in the diff buffer."
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")

    ;;; --- beg -- code-review specific code.
    ;;; I need to set a reference point for the first hunk header
    ;;; so the positioning of comments is done correctly.
    (let* ((path (code-review-db--curr-path code-review-pullreq-id))
           (path-name (oref path name))
           (head-pos (oref path head-pos))
           (at-pos-p (oref path at-pos-p)))
      (when (not head-pos)
        (let ((adjusted-pos (+ 1 (line-number-at-pos))))
          (code-review-db--curr-path-head-pos-update code-review-pullreq-id path-name adjusted-pos)
          (setq head-pos adjusted-pos)
          (setq path-name path-name))))
    ;;; --- end -- code-review specific code.

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
          ;;; --- beg -- code-review specific code.
          ;;; code-review specific code.
          ;;; add code comments
          (let* ((head-pos
                  (code-review-db-get-curr-head-pos code-review-pullreq-id))
                 (comment-written-pos
                  (code-review-db-get-comment-written-pos code-review-pullreq-id))
                 (diff-pos (- (line-number-at-pos)
                              head-pos
                              comment-written-pos))
                 (path-name (code-review-db--curr-path-name code-review-pullreq-id))
                 (path-pos (code-review-utils--comment-key path-name diff-pos)))
            (if-let (grouped-comments (and
                                       (not (code-review-db--comment-already-written?
                                             code-review-pullreq-id
                                             path-pos))
                                       (code-review-utils--comment-get
                                        code-review-section-grouped-comments
                                        path-pos)))
                (progn
                  (code-review-db--curr-path-comment-written-update
                   code-review-pullreq-id
                   path-pos)
                  (code-review-section-insert-comment grouped-comments))
              (forward-line))))
        ;;; --- end -- code-review specific code.
        (oset section end (point))
        (oset section washer 'magit-diff-paint-hunk)
        (oset section combined combined)
        (if combined
            (oset section from-ranges (butlast ranges))
          (oset section from-range (car ranges)))
        (oset section to-range (car (last ranges)))
        (oset section about about)))
    t))


(defmacro code-review-section--with-buffer (buff-name &rest body)
  "Include BODY in the buffer named BUFF-NAME."
  (declare (indent 0))
  `(let ((buffer (get-buffer-create ,buff-name)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (code-review-mode)
         (magit-insert-section (review-buffer)
           ,@body)))
     buffer))

(defun code-review-section--build-buffer (obj)
  "Build code review buffer given an OBJ."
  (advice-add 'magit-diff-insert-file-section
              :override #'code-review-section--magit-diff-insert-file-section)
  (advice-add 'magit-diff-wash-hunk
              :override #'code-review-section--magit-diff-wash-hunk)
  (deferred:$
    (deferred:parallel
      (lambda () (code-review-diff-deferred obj))
      (lambda () (code-review-infos-deferred obj)))
    (deferred:nextc it
      (lambda (x)
        (let-alist (-second-item x)
          (let* ((pull-request .data.repository.pullRequest)
                 (grouped-comments (code-review-comment-make-group pull-request))
                 (sha .data.repository.pullRequest.headRef.target.oid))
            (setq code-review-pullreq-info pull-request
                  code-review-section-grouped-comments grouped-comments
                  code-review-pullreq-id (oref obj pullreq-id))

            (code-review-db--pullreq-sha-update (oref obj pullreq-id) sha)

            (switch-to-buffer-other-window
             (code-review-section--with-buffer
               code-review-buffer-name
               (progn
                 (save-excursion
                   (insert (a-get (-first-item x) 'message))
                   (insert ?\n))
                 (magit-insert-section (code-review)
                   (magit-run-section-hook 'code-review-sections-hook))

                 (magit-wash-sequence
                  (apply-partially #'magit-diff-wash-diff ())))))

            (goto-char (point-min))

            (advice-remove 'magit-diff-insert-file-section
                           #'code-review-section--magit-diff-insert-file-section)
            (advice-remove 'magit-diff-wash-hunk
                           #'code-review-section--magit-diff-wash-hunk)))))
    (deferred:error it
      (lambda (err)
        (message "Got an error from your VC provider %S!" err)))))

(defun code-review-section--build-commit-buffer (obj)
  "Build code review buffer given an OBJ."
  (advice-add 'magit-diff-insert-file-section
              :override #'code-review-section--magit-diff-insert-file-section)
  (advice-add 'magit-diff-wash-hunk
              :override #'code-review-section--magit-diff-wash-hunk)
  (deferred:$
    (deferred:parallel
      (lambda () (code-review-commit-diff-deferred obj))
      (lambda () "nothing")
      (lambda () (code-review-infos-deferred obj)))
    (deferred:nextc it
      (lambda (x)
        (let-alist (-third-item x)
          (let* ((pull-request .data.repository.pullRequest))

            (setq code-review-pullreq-info pull-request
                  code-review-section-commit-comments nil
                  code-review-section-grouped-comments nil
                  code-review-pullreq-id (oref obj pullreq-id))

            (switch-to-buffer
             (code-review-section--with-buffer
               "*Code Review Commit*"
               (progn
                 (save-excursion
                   (insert (a-get (-first-item x) 'message))
                   (insert ?\n))

                 (magit-insert-section (code-review)
                   (magit-run-section-hook 'code-review-sections-hook))

                 (magit-wash-sequence
                  (apply-partially #'magit-diff-wash-diff ())))))

            (goto-char (point-min))

            (advice-remove 'magit-diff-insert-file-section
                           #'code-review-section--magit-diff-insert-file-section)
            (advice-remove 'magit-diff-wash-hunk
                           #'code-review-section--magit-diff-wash-hunk)))))
    (deferred:error it
      (lambda (err)
        (message "Got an error from your VC provider %S!" err)))))

;; TODO: commit description
;; TODO: commit comments

(defun code-review-section-insert-feedback (feedback)
  "Add review FEEDBACK."
  (with-current-buffer (get-buffer "*Code Review*")
    (save-excursion
      (goto-char (point-min))
      (magit-wash-sequence
       (lambda ()
         (with-slots (type value) (magit-current-section)
           (if (string-equal type 'feedback-text)
               (let ((inhibit-read-only t))
                 ;;; improve this to abort going over the whole buffer after we add the text
                 (delete-region (line-beginning-position) (line-end-position))
                 (insert feedback))
             (forward-line))))))))

(defun code-review-section-insert-local-comment (local-comment metadata)
  "Insert a LOCAL-COMMENT and attach section METADATA."
  (with-current-buffer (get-buffer "*Code Review*")
    (let ((inhibit-read-only t))
      (let-alist metadata
        (if .editing?
            (progn
              (goto-char .start)
              (when (not (looking-at "\\[local comment\\]"))
                (forward-line -1))
              (delete-region (point) .end))
          (progn
            (goto-char .cursor-pos)
            (forward-line)))
        (magit-insert-section (local-comment-header metadata)
          (magit-insert-heading
            (format "[local comment] - @%s:" (code-review-utils--git-get-user)))
          (magit-insert-section (local-comment metadata)
            (insert (string-trim local-comment))
            (insert ?\n)))))))

(defun code-review-section-delete-local-comment ()
  "Delete a local comment."
  (with-current-buffer (get-buffer "*Code Review*")
    (let ((inhibit-read-only t))
      (with-slots (type start end) (magit-current-section)
        (if (-contains-p '(local-comment
                           local-comment-header)
                         type)
            (progn
              (goto-char start)
              (when (not (looking-at "\\[local comment\\]"))
                (forward-line -1))
              (delete-region (point) end))
          (message "You can only delete local comments."))))))

(provide 'code-review-section)
;;; code-review-section.el ends here
