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

;; fix unbound symbols
(defvar magit-root-section)
(defvar code-review-buffer-name)
(defvar code-review-commit-buffer-name)

(defvar code-review-full-refresh? nil
  "Indicate if we want to perform a complete restart.
For internal usage only.")

(defvar code-review-grouped-comments nil
  "Hold grouped comments to avoid computation on every hunk line.
For internal usage only.")

(defvar code-review-hold-written-comment-ids nil
  "List to hold written comments ids.
For internal usage only.")

(defvar code-review-hold-written-comment-count nil
  "List of number of lines of comments written in the buffer.
For internal usage only.")

;;; headers

(defun code-review-section-insert-header-title ()
  "Insert the title header line."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (setq header-line-format
            (propertize
             (format "#%s: %s".number .title)
             'font-lock-face
             'magit-section-heading)))))

;; (code-review-db--pullreq-raw-infos)
;; (code-review-db-get-pullreq)
(defun code-review-section-insert-title ()
  "Insert the title of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review:title .title)
        (insert (format "%-17s" "Title: ") .title)
        (insert ?\n)))))

(defun code-review-section-insert-state ()
  "Insert the state of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review:state .state)
        (insert (format "%-17s" "State: ") (or (format "%s" .state) "none"))
        (insert ?\n)))))

(defun code-review-section-insert-ref ()
  "Insert the state of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review:ref `((base . ,.baseRefName)
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
             (perc (if (string-empty-p .milestone.progressPercentage) nil .milestone.progressPercentage))
             (milestone (if title
                            (format "%s (%s%%)" .milestone.title .milestone.progressPercentage)
                          "No milestone")))

        (magit-insert-section (code-review:milestone `((title . ,.milestone.title)
                                                       (progress . ,.milestone.progressPercentage)))
          (insert (format "%-17s" "Milestone: "))
          (insert (propertize milestone 'font-lock-face 'magit-dimmed))
          (insert ?\n))))))

(defun code-review-section-insert-labels ()
  "Insert the labels of the header buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review:labels .labels.nodes)
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
        (magit-insert-section (code-review:assignee assignees)
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
        (magit-insert-section (code-review:project projects)
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
        (magit-insert-section (code-review:reviewers suggested-reviewers)
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
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
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
  (let ((feedback (code-review-db--pullreq-feedback)))
    (magit-insert-section (code-review:feedback-header)
      (insert (propertize "Your Review Feedback" 'font-lock-face 'magit-section-heading))
      (magit-insert-heading)
      (magit-insert-section (code-review:feedback `((feedback . ,feedback)))
        (if feedback
            (insert feedback)
          (insert (propertize "Leave a comment here." 'font-lock-face 'magit-dimmed))))
      (insert ?\n)
      (insert ?\n))))

(defun code-review-section-insert-general-comments ()
  "Insert general comments for the PULL-REQUEST in the buffer."
  (when-let (infos (code-review-db--pullreq-raw-infos))
    (let-alist infos
      (magit-insert-section (code-review:conversation-header)
        (insert (propertize "Conversation" 'font-lock-face 'magit-section-heading))
        (magit-insert-heading)
        (dolist (c (append .comments.nodes (-filter
                                            (lambda (n)
                                              (not
                                               (string-empty-p (a-get n 'bodyText))))
                                            .reviews.nodes)))
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

(defun code-review-section-insert-outdated-comment (comments amount-loc)
  "Insert outdated COMMENTS in the buffer of PULLREQ-ID considering AMOUNT-LOC."

  ;;; hunk groups are necessary because we usually have multiple reviews about
  ;;; the same original position accross different commits snapshots.
  ;;; as github UI we will add those hunks and its comments
  (let* ((hunk-groups (-group-by (lambda (el) (a-get el 'diffHunk)) comments))
         (hunks (a-keys hunk-groups)))
    (dolist (hunk hunks)
      (let* ((diff-hunk-lines (split-string hunk "\n"))
             (amount-new-loc (+ 1 (length diff-hunk-lines)))
             (first-hunk-commit (-first-item (alist-get hunk hunk-groups nil nil 'equal)))
             (metadata1 `((comment . ,first-hunk-commit)
                          (amount-loc ., (+ amount-loc amount-new-loc)))))

        (code-review-db--curr-path-comment-count-update amount-new-loc)

        (magit-insert-section (code-review:outdated-hunk-header metadata1)
          (let ((heading (format "Reviewed - [OUTDATED]")))
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
                (let* ((body-lines (code-review-utils--split-comment (a-get c 'bodyText)))
                       (amount-new-loc2 (+ 2 (length body-lines)))
                       (metadata2 `((comment . ,c)
                                    (amount-loc .,(+ amount-loc amount-new-loc amount-new-loc2)))))
                  (code-review-db--curr-path-comment-count-update amount-new-loc2)

                  (magit-insert-section (code-review:outdated-comment-header metadata2)
                    (magit-insert-heading (format "Reviewed by %s[%s]:"
                                                  (a-get c 'author)
                                                  (a-get c 'state)))
                    (magit-insert-section (code-review:outdated-comment metadata2)
                      (dolist (l body-lines)
                        (insert l)
                        (insert ?\n))))
                  (insert ?\n))))))))))

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

            (setq code-review-hold-written-comment-count
                  (code-review-utils--comment-update-written-count
                   code-review-hold-written-comment-count
                   (a-get c 'path)
                   amount-loc-incr))

            (when (and (a-get c 'local?) (not (a-get c 'reply?)))
              ;;; when we create a local comment there is a -1 shift in the diff position
              ;;; to account for the last line of the buffer.. this will make it right at writing time.
              (forward-line))

            (cond
             ((a-get c 'reply?)
              (magit-insert-section (code-review:reply-comment-header metadata)
                (let ((heading "Reply by YOU: "))
                  (add-face-text-property 0 (length heading) 'code-review-recent-comment-heading t heading)
                  (magit-insert-heading heading))
                (magit-insert-section (code-review:reply-comment metadata)
                  (dolist (l body-lines)
                    (insert l)
                    (insert "\n"))
                  (insert ?\n))))
             ((a-get c 'local?)
              (magit-insert-section (code-review:local-comment-header metadata)
                (let ((heading "Comment by YOU: "))
                  (add-face-text-property 0 (length heading) 'code-review-recent-comment-heading t heading)
                  (magit-insert-heading heading))
                (magit-insert-section (code-review:local-comment metadata)
                  (dolist (l body-lines)
                    (insert l)
                    (insert "\n"))
                  (insert ?\n))))
             (t
              (magit-insert-section (code-review:comment-header metadata)
                (let ((heading (format "Reviewed by @%s [%s]: " (a-get c 'author) (a-get c 'state))))
                  (add-face-text-property 0 (length heading) 'code-review-recent-comment-heading t heading)
                  (magit-insert-heading heading))
                (magit-insert-section (code-review:comment metadata)
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
  (code-review-db--curr-path-update (substring-no-properties file))
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
           (head-pos (oref path head-pos))
           (at-pos-p (oref path at-pos-p)))
      (when (not head-pos)
        (let ((adjusted-pos (+ 1 (line-number-at-pos))))
          (code-review-db--curr-path-head-pos-update path-name adjusted-pos)
          (setq head-pos adjusted-pos)
          (setq path-name path-name)))
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
                    (or (alist-get path-name code-review-hold-written-comment-count nil nil 'equal) 0))
                   (diff-pos (- (line-number-at-pos)
                                head-pos
                                comment-written-pos))
                   (path-pos (code-review-utils--comment-key path-name diff-pos))
                   (written? (member path-pos code-review-hold-written-comment-ids))
                   (grouped-comment (code-review-utils--comment-get
                                     code-review-grouped-comments
                                     path-pos)))
              (if (and (not written?) grouped-comment)
                  (progn
                    (push path-pos code-review-hold-written-comment-ids)
                    (code-review-section-insert-comment
                     grouped-comment
                     comment-written-pos))
                (forward-line))))

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

(defun code-review-section--trigger-hooks (buff-name &optional window-config commit-focus?)
  "Trigger magit section hooks, draw BUFF-NAME respecting WINDOW-CONFIG.
Run code review commit buffer hook when COMMIT-FOCUS? is non-nil."
  (unwind-protect
      (progn
        ;; advices
        (advice-add 'magit-diff-insert-file-section :override #'code-review-section--magit-diff-insert-file-section)
        (advice-add 'magit-diff-wash-hunk :override #'code-review-section--magit-diff-wash-hunk)

        (setq code-review-grouped-comments
              (code-review-comment-make-group
               (code-review-db--pullreq-raw-comments))
              code-review-hold-written-comment-count nil
              code-review-hold-written-comment-ids nil)

        (let ((buff (code-review-section--with-buffer
                      buff-name
                      (progn
                        (save-excursion
                          (insert (code-review-db--pullreq-raw-diff))
                          (insert ?\n)
                          (insert ?\n))

                        (magit-insert-section (code-review)
                          (if commit-focus?
                              (magit-run-section-hook 'code-review-sections-commit-hook)
                            (magit-run-section-hook 'code-review-sections-hook)))

                        (magit-wash-sequence
                         (apply-partially #'magit-diff-wash-diff ()))))))

          (when window-config
            (set-window-configuration window-config))

          buff))
    ;; remove advices
    (advice-remove 'magit-diff-insert-file-section #'code-review-section--magit-diff-insert-file-section)
    (advice-remove 'magit-diff-wash-hunk #'code-review-section--magit-diff-wash-hunk)))

(defun code-review-section--build-buffer (&optional not-switch-buffer?)
  "Build code review buffer given an OBJ."
  (let ((obj (code-review-db-get-pullreq)))
    (deferred:$
      (deferred:parallel
        (lambda () (code-review-diff-deferred obj))
        (lambda () (code-review-infos-deferred obj)))
      (deferred:nextc it
        (lambda (x)
          (let-alist (-second-item x)

            (when code-review-full-refresh?
              (code-review-db--pullreq-raw-infos-update
               .data.repository.pullRequest)
              (code-review-db--pullreq-raw-diff-update
               (a-get (-first-item x) 'message)))

            (let ((resp-buffer
                   (code-review-section--trigger-hooks
                    code-review-buffer-name)))
              (if not-switch-buffer?
                  resp-buffer
                (progn
                  (switch-to-buffer-other-window
                   resp-buffer)
                  (goto-char (point-min))))))))
      (deferred:error it
        (lambda (err)
          (message "Got an error from your VC provider %S!" err))))))

(defun code-review-section--build-commit-buffer ()
  "Build code review buffer given an OBJ."
  (let ((obj (code-review-db-get-pullreq)))
    (deferred:$
      (deferred:parallel
        (lambda () (code-review-commit-diff-deferred obj))
        (lambda () "nothing")
        (lambda () (code-review-infos-deferred obj)))
      (deferred:nextc it
        (lambda (x)
          (let-alist (-third-item x)

            (when code-review-full-refresh?
              (code-review-db--pullreq-raw-infos-update
               .data.repository.pullRequest)
              (code-review-db--pullreq-raw-diff-update
               (a-get (-first-item x) 'message)))

            (switch-to-buffer
             (code-review-section--trigger-hooks
              code-review-commit-buffer-name
              nil
              t))
            (code-review-commit-minor-mode))))
      (deferred:error it
        (lambda (err)
          (message "Got an error from your VC provider %S!" err))))))

;; TODO: commit description
;; TODO: commit comments

(defun code-review-section-insert-local-comment (local-comment metadata window-config &optional commit-focus?)
  "Insert LOCAL-COMMENT and attach section METADATA then preserve WINDOW-CONFIG.
Using COMMIT-FOCUS? to enable add comment into commit review buffer."
  (with-current-buffer (get-buffer (if commit-focus?
                                       code-review-commit-buffer-name
                                     code-review-buffer-name))
    (let-alist metadata
      (let ((inhibit-read-only t)
            (pr (code-review-db-get-pullreq))
            (current-line-pos (save-excursion
                                (goto-char .cursor-pos)
                                (line-number-at-pos))))
        (magit-insert-section (code-review:local-comment-header metadata)
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
              (while (and (not (looking-at "Comment by\\|Reviewed by\\|modified"))
                          (not (equal (point) (point-min))))
                (forward-line -1))
              (let ((section (magit-current-section))
                    (amount-loc nil)
                    (path-name nil))
                (if (not section)
                    (setq amount-loc 0)
                  (with-slots (type value) section
                    (if (string-equal type "file")
                        (setq amount-loc 0
                              path-name (substring-no-properties value))
                      (setq amount-loc (a-get value 'amount-loc)
                            path-name (a-get-in value (list 'comment 'path))))
                    (let* ((diff-pos (+ (- current-line-pos
                                           amount-loc
                                           (code-review-db--head-pos path-name))
                                        0))
                           (reply? (a-get metadata 'reply?))
                           (reply-pos (when reply?
                                        (- (+ (a-get metadata 'position)
                                              (length (split-string (a-get metadata 'comment-text) "\n")))
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
                      (code-review-db--pullreq-raw-comments-update local-comment-record)))))))
          (erase-buffer)
          (code-review-section--trigger-hooks
           (if commit-focus?
               code-review-commit-buffer-name
             code-review-buffer-name)
           window-config))))))

(defun code-review-section-delete-local-comment ()
  "Delete a local comment."
  (with-current-buffer (get-buffer "*Code Review*")
    (let ((inhibit-read-only t))
      (with-slots (type value start end) (magit-current-section)
        (if (-contains-p '(code-review:local-comment
                           code-review:local-comment-header
                           code-review:reply-comment
                           code-review:reply-comment-header)
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
