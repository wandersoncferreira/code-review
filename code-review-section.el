;;; code-review-section.el --- UI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.1
;; Homepage: https://github.com/wandersoncferreira/code-review
;; Package-Requires: ((emacs "25.1"))
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
;;  Code to build the UI.
;;
;;; Code:

(require 'deferred)
(require 'magit-section)
(require 'magit-diff)
(require 'code-review-core)
(require 'code-review-db)
(require 'code-review-utils)

;; fix unbound symbols
(defvar magit-root-section)
(defvar code-review-buffer-name)
(defvar code-review-commit-buffer-name)
(defvar code-review-comment-cursor-pos)
(defvar code-review-commit-minor-mode)
(defvar code-review-mode)

(defvar code-review-section-full-refresh? nil
  "Indicate if we want to perform a complete restart.
For internal usage only.")

(defvar code-review-section-grouped-comments nil
  "Hold grouped comments to avoid computation on every hunk line.
For internal usage only.")

(defvar code-review-section-hold-written-comment-ids nil
  "List to hold written comments ids.
For internal usage only.")

(defvar code-review-section-hold-written-comment-count nil
  "List of number of lines of comments written in the buffer.
For internal usage only.")

;;; headers

(defun code-review-section-insert-header-title ()
  "Insert the title header line."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (setq header-line-format
            (propertize
             (format "#%s: %s".number (code-review-db--pullreq-title))
             'font-lock-face
             'magit-section-heading)))))

;;; TODO: add some nice face to true and false
(defun code-review-section-insert-is-draft ()
  "Insert the isDraft value of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let ((draft? (if .isDraft "true" "false")))
        (magit-insert-section (code-review-is-draft draft?)
          (insert (format "%-17s" "Draft: ") draft?)
          (insert ?\n))))))

(defun code-review-section-insert-title ()
  "Insert the title of the header buffer."
  (when-let (title (code-review-db--pullreq-title))
    (magit-insert-section (code-review-title title)
      (insert (format "%-17s" "Title: ") title)
      (insert ?\n))))

(defun code-review-section-insert-state ()
  "Insert the state of the header buffer."
  (when-let (state (code-review-db--pullreq-state))
    (magit-insert-section (code-review-state state)
      (insert (format "%-17s" "State: ") (or (format "%s" state) "none"))
      (insert ?\n))))

(defun code-review-section-insert-ref ()
  "Insert the state of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review-ref `((base . ,.baseRefName)
                                               (head . ,.headRefName)))
        (insert (format "%-17s" "Refs: "))
        (insert .baseRefName)
        (insert (propertize " ... " 'font-lock-face 'magit-dimmed))
        (insert .headRefName)
        (insert ?\n)))))

(defun code-review-section-insert-milestone ()
  "Insert the milestone of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((title (if (string-empty-p .milestone.title) nil .milestone.title))
             (milestone (cond
                         ((and title .milestone.progressPercentage)
                          (format "%s (%s%%)" .milestone.title .milestone.progressPercentage))
                         (title
                          (format "%s" .milestone.title))
                         (t
                          "No milestone"))))

        (magit-insert-section (code-review-milestone `((title . ,.milestone.title)
                                                       (progress . ,.milestone.progressPercentage)
                                                       (visible-text . ,milestone)))
          (insert (format "%-17s" "Milestone: "))
          (insert (propertize milestone 'font-lock-face 'magit-dimmed))
          (insert ?\n))))))

(defun code-review-section-insert-labels ()
  "Insert the labels of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review-labels .labels.nodes)
        (insert (format "%-17s" "Labels: "))
        (if .labels.nodes
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
          (insert (propertize "None yet" 'font-lock-face 'magit-dimmed)))
        (insert ?\n)))))

(defun code-review-section-insert-assignee ()
  "Insert the assignee of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((assignee-names (-map
                              (lambda (a)
                                (format "%s (@%s)"
                                        (a-get a 'name)
                                        (a-get a 'login)))
                              .assignees.nodes))
             (assignees (if assignee-names
                            (string-join assignee-names ", ")
                          (propertize "No one — Assign yourself" 'font-lock-face 'magit-dimmed))))
        (magit-insert-section (code-review-assignee assignees)
          (insert (format "%-17s" "Assignees: ") assignees)
          (insert ?\n))))))

(defun code-review-section-insert-project ()
  "Insert the project of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((project-names (-map
                             (lambda (p)
                               (a-get-in p (list 'project 'name)))
                             .projectCards.nodes))
             (projects (if project-names
                           (string-join project-names ", ")
                         (propertize "None yet" 'font-lock-face 'magit-dimmed))))
        (magit-insert-section (code-review-project projects)
          (insert (format "%-17s" "Projects: ") projects)
          (insert ?\n))))))

(defun code-review-section-insert-suggested-reviewers ()
  "Insert the suggested reviewers."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (let* ((reviewers (string-join
                         (-map
                          (lambda (r)
                            (a-get-in r (list 'reviewer 'name)))
                          .suggestedReviewers)
                         ", "))
             (suggested-reviewers (if (string-empty-p reviewers)
                                      (propertize "No reviews" 'font-lock-face 'magit-dimmed)
                                    reviewers)))
        (magit-insert-section (code-review-reviewers suggested-reviewers)
          (insert (format "%-17s" "Suggested-Reviewers: ") suggested-reviewers)
          (insert ?\n))))))

(defun code-review-section-insert-headers ()
  "Insert all the headers."
  (magit-insert-headers 'code-review-headers-hook))

;;; next sections

(defun code-review-section-insert-commits ()
  "Insert commits from PULL-REQUEST."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review-commits-header)
        (insert (propertize "Commits" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (magit-insert-section (code-review-commits-body)
          (dolist (c .commits.nodes)
            (let ((commit-value `((sha ,(a-get-in c (list 'commit 'abbreviatedOid))))))
              (magit-insert-section (code-review-commit commit-value)
                (insert (propertize
                         (format "%-6s " (a-get-in c (list 'commit 'abbreviatedOid)))
                         'font-lock-face 'magit-hash)
                        (a-get-in c (list 'commit 'message)))))
            (insert ?\n)))
        (insert ?\n)))))


(defun code-review-section-insert-pr-description ()
  "Insert PULL-REQUEST description."
  (when-let (description (code-review-db--pullreq-description))
    (let ((description-cleaned (if (string-empty-p description)
                                   "No description provided."
                                 description)))
      (magit-insert-section (code-review-pr-description-header)
        (insert (propertize "Description" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (magit-insert-section (code-review-pr-description description-cleaned)
          (if (string-empty-p description)
              (insert (propertize description-cleaned 'font-lock-face 'magit-dimmed))
            (insert description-cleaned))
          (insert ?\n)
          (insert ?\n))))))

(defun code-review-section-insert-feedback-heading ()
  "Insert feedback heading."
  (let ((feedback (code-review-db--pullreq-feedback)))
    (magit-insert-section (code-review-feedback-header)
      (insert (propertize "Your Review Feedback" 'font-lock-face 'magit-section-heading))
      (magit-insert-heading)
      (magit-insert-section (code-review-feedback `((feedback . ,feedback)))
        (if feedback
            (insert feedback)
          (insert (propertize "Leave a comment here." 'font-lock-face 'magit-dimmed))))
      (insert ?\n)
      (insert ?\n))))

(defun code-review-section-insert-general-comments ()
  "Insert general comments for the PULL-REQUEST in the buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review-conversation-header)
        (insert (propertize "Conversation" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (dolist (c (append .comments.nodes (-filter
                                            (lambda (n)
                                              (not
                                               (string-empty-p (a-get n 'bodyText))))
                                            .reviews.nodes)))
          (magit-insert-section (code-review-general-comment c)
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

(defun code-review-section-insert-outdated-comment (comments amount-loc)
  "Insert outdated COMMENTS in the buffer of PULLREQ-ID considering AMOUNT-LOC."
  ;;; hunk groups are necessary because we usually have multiple reviews about
  ;;; the same original position accross different commits snapshots.
  ;;; as github UI we will add those hunks and its comments
  (let* ((hunk-groups (-group-by (lambda (el) (a-get el 'diffHunk)) comments))
         (hunks (a-keys hunk-groups)))
    (dolist (hunk hunks)
      (when (not hunk)
        (code-review-utils--log
         "code-review-section-insert-outdated-comment"
         (format "Every outdated comment must have a hunk! Error found for %S"
                 (prin1-to-string hunk)))
        (message "Hunk empty found. A empty string will be used instead. Report this bug please."))
      (let* ((safe-hunk (or hunk ""))
             (diff-hunk-lines (split-string safe-hunk "\n"))
             (amount-new-loc (+ 1 (length diff-hunk-lines)))
             (first-hunk-commit (-first-item (alist-get safe-hunk hunk-groups nil nil 'equal)))
             (metadata1 `((comment . ,first-hunk-commit)
                          (amount-loc ., (+ amount-loc amount-new-loc)))))

        (setq code-review-section-hold-written-comment-count
              (code-review-utils--comment-update-written-count
               code-review-section-hold-written-comment-count
               (a-get first-hunk-commit 'path)
               amount-new-loc))

        (magit-insert-section (code-review-outdated-hunk-header metadata1)
          (let ((heading (format "Reviewed - [OUTDATED]")))
            (add-face-text-property 0 (length heading)
                                    'code-review-outdated-comment-heading
                                    t heading)
            (magit-insert-heading heading)
            (magit-insert-section ()
              (save-excursion
                (insert safe-hunk))
              (magit-diff-wash-hunk)
              (insert ?\n)

              (dolist (c (alist-get safe-hunk hunk-groups nil nil 'equal))
                (let* ((body-lines (code-review-utils--split-comment (a-get c 'bodyText)))
                       (amount-new-loc-outdated (+ 2 (length body-lines)))
                       (metadata2 `((comment . ,c)
                                    (amount-loc .,(+ amount-loc amount-new-loc amount-new-loc-outdated)))))

                  (setq code-review-section-hold-written-comment-count
                        (code-review-utils--comment-update-written-count
                         code-review-section-hold-written-comment-count
                         (a-get first-hunk-commit 'path)
                         amount-new-loc-outdated))

                  (magit-insert-section (code-review-outdated-comment-header metadata2)
                    (magit-insert-heading (format "Reviewed by %s[%s]:"
                                                  (a-get c 'author)
                                                  (a-get c 'state)))
                    (magit-insert-section (code-review-outdated-comment metadata2)
                      (dolist (l body-lines)
                        (insert l)
                        (insert ?\n))))
                  (insert ?\n))))))))))

(defun code-review-section-insert-outdated-comment-missing (path-name missing-paths grouped-comments)
  "Write missing outdated comments in the end of the current path.
We need PATH-NAME, MISSING-PATHS, and GROUPED-COMMENTS to make this work."
  (dolist (path-pos missing-paths)
    (let ((comment-written-pos
           (or (alist-get path-name code-review-section-hold-written-comment-count nil nil 'equal)
               0)))
      (code-review-section-insert-outdated-comment
       (code-review-utils--comment-get
        grouped-comments
        path-pos)
       comment-written-pos)
      (push path-pos code-review-section-hold-written-comment-ids))))

(defun code-review-section-insert-comment (comments amount-loc)
  "Insert COMMENTS to PULLREQ-ID keep the AMOUNT-LOC of comments written.
A quite good assumption: every comment in an outdated hunk will be outdated."
  (if (a-get (-first-item comments) 'outdated)
      (code-review-section-insert-outdated-comment
       comments
       amount-loc)
    (let ((new-amount-loc amount-loc))
      (dolist (c comments)
        (let* ((body-lines (code-review-utils--split-comment (a-get c 'bodyText)))
               (amount-loc-incr (+ 2 (length body-lines))))

          (setq new-amount-loc (+ new-amount-loc amount-loc-incr))

          (let ((metadata
                 `((comment . ,c)
                   (amount-loc . ,new-amount-loc))))

            (setq code-review-section-hold-written-comment-count
                  (code-review-utils--comment-update-written-count
                   code-review-section-hold-written-comment-count
                   (a-get c 'path)
                   amount-loc-incr))

            (when (and (a-get c 'local?) (not (a-get c 'reply?)))
              ;;; when we create a local comment there is a -1 shift in the diff position
              ;;; to account for the last line of the buffer.. this will make it right at writing time.
              (forward-line))

            (cond
             ((a-get c 'reply?)
              (magit-insert-section (code-review-reply-comment-header metadata)
                (let ((heading "Reply by YOU: "))
                  (add-face-text-property 0 (length heading) 'code-review-recent-comment-heading t heading)
                  (magit-insert-heading heading))
                (magit-insert-section (code-review-reply-comment metadata)
                  (dolist (l body-lines)
                    (insert l)
                    (insert "\n"))
                  (insert ?\n))))
             ((a-get c 'local?)
              (magit-insert-section (code-review-local-comment-header metadata)
                (let ((heading "Comment by YOU: "))
                  (add-face-text-property 0 (length heading) 'code-review-recent-comment-heading t heading)
                  (magit-insert-heading heading))
                (magit-insert-section (code-review-local-comment metadata)
                  (dolist (l body-lines)
                    (insert l)
                    (insert "\n"))
                  (insert ?\n))))
             (t
              (magit-insert-section (code-review-comment-header metadata)
                (let ((heading (format "Reviewed by @%s [%s]: " (a-get c 'author) (a-get c 'state))))
                  (add-face-text-property 0 (length heading) 'code-review-recent-comment-heading t heading)
                  (magit-insert-heading heading))
                (magit-insert-section (code-review-comment-body metadata)
                  (dolist (l body-lines)
                    (insert l)
                    (insert "\n"))
                  (insert ?\n)))))))))))

(defun code-review-section--magit-diff-insert-file-section
    (file orig status modes rename header &optional long-status)
  "Overwrite the original Magit function on `magit-diff.el' FILE."

  ;;; --- beg -- code-review specific code.
  ;;; I need to set a reference point for the first hunk header
  ;;; so the positioning of comments is done correctly.
  (let* ((raw-path-name (substring-no-properties file))
         (clean-path (if (string-prefix-p "b/" raw-path-name)
                         (replace-regexp-in-string "^b\\/" "" raw-path-name)
                       raw-path-name)))
    (code-review-db--curr-path-update clean-path))
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
Code Review inserts PR comments sections in the diff buffer.
Argument GROUPED-COMMENTS comments grouped by path and diff position."
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")


    ;;; FIXME: Very problematic to have any function dependent on DATABASE here
    ;;; because this function will be run for every single line of the DIFF
    ;;; ... better to rely on defvar-local variables.

    ;;; --- beg -- code-review specific code.
    ;;; I need to set a reference point for the first hunk header
    ;;; so the positioning of comments is done correctly.
    (let* ((path (code-review-db--curr-path))
           (path-name (oref path name))
           (head-pos (oref path head-pos)))
      (when (not head-pos)
        (let ((adjusted-pos (+ 1 (line-number-at-pos))))
          (code-review-db--curr-path-head-pos-update path-name adjusted-pos)
          (setq head-pos adjusted-pos)
          (setq path-name path-name)))

      (when (not head-pos)
        (code-review-utils--log
         "code-review-section--magit-diff-wash-hunk"
         (format "Every diff is associated with a PATH (the file). Head pos nil for %S"
                 (prin1-to-string path)))
        (message "ERROR: Head position for path %s was not found.
Please Report this Bug" path-name))

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
            (let* ((comment-written-pos
                    (or (alist-get path-name code-review-section-hold-written-comment-count nil nil 'equal) 0))
                   (diff-pos (- (line-number-at-pos)
                                (or head-pos 0)
                                comment-written-pos))
                   (path-pos (code-review-utils--comment-key path-name diff-pos))
                   (written? (-contains-p code-review-section-hold-written-comment-ids path-pos))
                   (grouped-comment (code-review-utils--comment-get
                                     code-review-section-grouped-comments
                                     path-pos)))
              (if (and (not written?) grouped-comment)
                  (progn
                    (push path-pos code-review-section-hold-written-comment-ids)
                    (code-review-section-insert-comment
                     grouped-comment
                     comment-written-pos))
                (forward-line))))

          ;;; we can have outdated comments missing that were written in a
          ;;; version of the buffer that had more lines than now.
          ;;; call function to write the remaining comments.
          ;;; important to only consider comments for this path.
          (when-let (missing-paths (code-review-utils--missing-outdated-commments?
                                    path-name
                                    code-review-section-hold-written-comment-ids
                                    code-review-section-grouped-comments))
            (code-review-section-insert-outdated-comment-missing
             path-name
             missing-paths
             code-review-section-grouped-comments))

        ;;; --- end -- code-review specific code.
          (oset section end (point))
          (oset section washer 'magit-diff-paint-hunk)
          (oset section combined combined)
          (if combined
              (oset section from-ranges (butlast ranges))
            (oset section from-range (car ranges)))
          (oset section to-range (car (last ranges)))
          (oset section about about))))
    t))

(defun code-review-section--trigger-hooks (buff-name &optional commit-focus?)
  "Trigger magit section hooks and draw BUFF-NAME.
Run code review commit buffer hook when COMMIT-FOCUS? is non-nil."
  (unwind-protect
      (progn
        ;; advices
        (advice-add 'magit-diff-insert-file-section :override #'code-review-section--magit-diff-insert-file-section)
        (advice-add 'magit-diff-wash-hunk :override #'code-review-section--magit-diff-wash-hunk)

        (setq code-review-section-grouped-comments
              (code-review-comment-make-group
               (code-review-db--pullreq-raw-comments))
              code-review-section-hold-written-comment-count nil
              code-review-section-hold-written-comment-ids nil)

        (with-current-buffer (get-buffer-create buff-name)
          (let* ((window (get-buffer-window buff-name))
                 (ws (window-start window))
                 (inhibit-read-only t))
            (save-excursion
              (erase-buffer)
              (insert (code-review-db--pullreq-raw-diff))
              (insert ?\n)
              (insert ?\n))
            (magit-insert-section (review-buffer)
              (magit-insert-section (code-review)
                (if commit-focus?
                    (magit-run-section-hook 'code-review-sections-commit-hook)
                  (magit-run-section-hook 'code-review-sections-hook)))
              (magit-wash-sequence
               (apply-partially #'magit-diff-wash-diff ())))
            (if window
                (progn
                  (pop-to-buffer buff-name)
                  (set-window-start window ws)
                  (when code-review-comment-cursor-pos
                    (goto-char code-review-comment-cursor-pos)))
              (progn
                (switch-to-buffer-other-window buff-name)
                (goto-char (point-min))))
            (if commit-focus?
                (code-review-commit-minor-mode)
              (code-review-mode))
            (code-review-section-insert-header-title))))

    ;; remove advices
    (advice-remove 'magit-diff-insert-file-section #'code-review-section--magit-diff-insert-file-section)
    (advice-remove 'magit-diff-wash-hunk #'code-review-section--magit-diff-wash-hunk)))


(defun code-review-section--build-buffer (buff-name &optional commit-focus?)
  "Build BUFF-NAME set COMMIT-FOCUS? mode to use commit list of hooks."

  (if (not code-review-section-full-refresh?)
      (code-review-section--trigger-hooks buff-name commit-focus?)
    (let ((obj (code-review-db-get-pullreq)))
      (deferred:$
        (deferred:parallel
          (lambda () (code-review-core-diff-deferred obj))
          (lambda () (code-review-core-infos-deferred obj)))
        (deferred:nextc it
          (lambda (x)
            (let-alist (-second-item x)
              (code-review-db--pullreq-raw-infos-update .data.repository.pullRequest)
              (code-review-db--pullreq-raw-diff-update
               (code-review-utils--clean-diff-prefixes
                (a-get (-first-item x) 'message)))
              (code-review-section--trigger-hooks buff-name))))
        (deferred:error it
          (lambda (err)
            (code-review-utils--log
             "code-review-section--build-buffer"
             (prin1-to-string err))
            (message "Got an error from your VC provider. Check `code-review-log-file'.")))))))

(defun code-review-section--build-commit-buffer (buff-name)
  "Build commit buffer review given by BUFF-NAME."
  (code-review-section--build-buffer buff-name t))

(defun code-review-section-insert-local-comment (local-comment metadata buff-name)
  "Insert LOCAL-COMMENT and attach section METADATA in BUFF-NAME."
  (with-current-buffer (get-buffer buff-name)
    (let-alist metadata
      (let ((current-line-pos (save-excursion
                                (goto-char .cursor-pos)
                                (line-number-at-pos))))
        (magit-insert-section (code-review-local-comment-header metadata)
          (if .editing?
              (progn
                (code-review-db-delete-raw-comment .comment.internal-id)
                (code-review-db--pullreq-raw-comments-update
                 `((author (login . ,(code-review-utils--git-get-user)))
                   (state . ,.comment.state)
                   (comments (nodes ((bodyText . ,.comment.bodyText)
                                     (outdated . nil)
                                     (reply? . ,.comment.reply?)
                                     (local? . ,.comment.local?)
                                     (path . ,.comment.path)
                                     (position . ,.comment.position)
                                     (databaseId . ,.comment.databaseId)
                                     (internal-id . ,(uuidgen-4))))))))
            (progn
              (goto-char .cursor-pos)
              (while (and (not (looking-at "Comment by\\|Reviewed by\\|modified\\|new file\\|deleted"))
                          (not (equal (point) (point-min))))
                (forward-line -1))
              (let ((section (magit-current-section))
                    (amount-loc nil)
                    (path-name nil))
                (if (not section)
                    (setq amount-loc 0)
                  (with-slots (type value) section
                    (if (string-equal type "file")
                        (let* ((raw-path-name (substring-no-properties value))
                               (clean-path (if (string-prefix-p "b/" raw-path-name)
                                               (replace-regexp-in-string "^b\\/" "" raw-path-name)
                                             raw-path-name)))
                          (setq amount-loc 0
                                path-name clean-path))
                      (let ((aml (a-get value 'amount-loc))
                            (pn (a-get-in value (list 'comment 'path))))

                        (when (not aml)
                          (code-review-utils--log
                           "code-review-section-insert-local-comment"
                           (format "Amount of lines of comment written not found. Report this bug. %S"
                                   (prin1-to-string section))))

                        (when (not pn)
                          (code-review-utils--log
                           "code-review-section-insert-local-comment"
                           (format "Path name not found in comment. Report this bug. %S"
                                   (prin1-to-string section))))

                        (setq amount-loc (or aml 0)
                              path-name (or pn ""))))
                    (let* ((diff-pos (+ (- current-line-pos
                                           amount-loc
                                           (code-review-db--head-pos path-name))
                                        0))
                           (reply? (a-get metadata 'reply?))
                           (reply-pos (when reply?
                                        (- (+ (or (a-get metadata 'position) 0)
                                              (length (split-string (or (a-get metadata 'comment-text) "") "\n")))
                                           1)))
                           (state (if reply? "REPLY COMMENT" "LOCAL COMMENT"))
                           (local-comment-record
                            `((author (login . ,(code-review-utils--git-get-user)))
                              (state . ,state)
                              (comments (nodes ((bodyText . ,local-comment)
                                                (outdated . nil)
                                                (reply? . ,reply?)
                                                (internal-id . ,(uuidgen-4))
                                                (local? . ,(if reply? nil t))
                                                (path . ,path-name)
                                                (position . ,(if reply? reply-pos diff-pos))
                                                (databaseId . ,(a-get metadata 'database-id))))))))
                      (code-review-db--pullreq-raw-comments-update local-comment-record))))))))))))

(defun code-review-section-delete-local-comment ()
  "Delete a local comment."
  (with-current-buffer (get-buffer "*Code Review*")
    (let ((inhibit-read-only t))
      (with-slots (type value start end) (magit-current-section)
        (if (-contains-p '(code-review-local-comment
                           code-review-local-comment-header
                           code-review-reply-comment
                           code-review-reply-comment-header)
                         type)
            (progn
              (code-review-db-delete-raw-comment
               (a-get-in value (list 'comment 'internal-id)))
              (goto-char start)
              (when (not (looking-at "\\[local comment\\]"))
                (forward-line -1))
              (delete-region (point) end))
          (message "You can only delete local comments."))))))

(provide 'code-review-section)
;;; code-review-section.el ends here
