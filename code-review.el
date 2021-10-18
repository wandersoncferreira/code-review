;;; code-review.el --- Perform code review from Github -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.1
;; Keywords: git, tools, vc
;; Homepage: https://github.com/wandersoncferreira/code-review
;; Package-Requires: ((emacs "25.1") (magit "3.3.0") (s "1.12.0") (ghub "2.0") (dash "2.11.0") (deferred "0.5.1") (a "0.1.1"))

;; This file is not part of GNU Emacs

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
;; `code-review` lets you submit code review with Emacs.
;;
;; Currently only supports Github but contains extension points to other forges.
;;

;;; Code:

(require 'magit-section)
(require 'code-review-section)
(require 'code-review-github)
(require 'code-review-comment)
(require 'code-review-utils)

(defconst code-review-buffer-name "*Code Review*")

(defvar code-review-pr-alist nil
  "For internal usage only.")

;;; Faces

(defface code-review-recent-comment-heading
  '((((supports (:box t))) :inherit magit-branch-remote :box t)
    (t                     :inherit magit-branch-remote :inverse-video t))
  "Face for recent comments"
  :group 'code-review)

(defface code-review-outdated-comment-heading
  '((((supports (:box t))) :inherit magit-cherry-equivalent :box t)
    (t                     :inherit magit-cherry-equivalent :inverse-video t))
  "Face for outdated comments"
  :group 'code-review)

(defmacro code-review-with-buffer (&rest body)
  "Include BODY in the buffer."
  (declare (indent 0))
  `(let ((buffer (get-buffer-create code-review-buffer-name)))
     (with-current-buffer buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (code-review-mode)
         (magit-insert-section (review-buffer)
           ,@body)))
     (switch-to-buffer-other-window buffer)))

(defun code-review-group-comments (pull-request)
  "Group comments in PULL-REQUEST to ease the access when building the buffer."
  (-reduce-from
   (lambda (acc node)
     (let ((author (a-get-in node (list 'author 'login)))
           (state (a-get node 'state)))
       (if-let (comments (a-get-in node (list 'comments 'nodes)))
           (-reduce-from
            (lambda (grouped-comments comment)
              (let-alist comment
                (let* ((comment-enriched (a-assoc comment 'author author 'state state))
                       (handled-pos (or .position .originalPosition))
                       (path-pos (code-review-utils--comment-key .path handled-pos)))
                  (if (or (not grouped-comments)
                          (not (code-review-utils--comment-get grouped-comments path-pos)))
                      (a-assoc grouped-comments path-pos (list comment-enriched))
                    (a-update grouped-comments path-pos (lambda (v) (append v (list comment-enriched))))))))
            acc
            comments)
         acc)))
   nil
   (a-get-in pull-request (list 'reviews 'nodes))))

(defun code-review-section-build-buffer (pr-alist)
  "Build code review buffer given a PR-ALIST with basic info about target repo."

  ;;; small set of stateful variables used around the project that need to be
  ;;; reset everytime we build the diff buffer.
  (setq code-review-section-first-hunk-header-pos nil
        code-review-section-written-comments-count nil
        code-review-section-written-comments-ident nil)

  (deferred:$
    (deferred:parallel
      (lambda () (code-review-github-get-diff-deferred pr-alist))
      (lambda () (code-review-github-get-pr-info-deferred pr-alist)))
    (deferred:nextc it
      (lambda (x)
        (let-alist (-second-item x)
          (let* ((pull-request .data.repository.pullRequest)
                 (grouped-comments (code-review-group-comments pull-request)))
            (code-review-with-buffer
              (magit-insert-section (demo)
                (save-excursion
                  (insert (a-get (-first-item x) 'message))
                  (insert "\n"))
                (setq header-line-format
                      (propertize
                       (format "#%s: %s"
                               (a-get pull-request 'number)
                               (a-get pull-request 'title))
                       'font-lock-face
                       'magit-section-heading))
                (setq code-review-pr-alist
                      (a-assoc pr-alist
                               'sha .data.repository.pullRequest.headRef.target.oid))
                (code-review-section-insert-headers pull-request)
                (code-review-section-insert-commits pull-request)
                (code-review-section-insert-pr-description pull-request)
                (code-review-section-insert-feedback-heading)
                (magit-wash-sequence
                 (apply-partially #'code-review-section-wash grouped-comments)))
              (goto-char (point-min)))))))
    (deferred:error it
      (lambda (err)
        (message "Got an error from your VC provider %S!" err)))))

(defun code-review-build-submit-structure ()
  "Return A-LIST with replies and reviews to submit."
  (let ((replies nil)
        (review-comments nil)
        (body nil))
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
    (let* ((partial-review `((commit_id . ,(a-get code-review-pr-alist 'sha))
                             (body . ,(a-get code-review-pr-alist 'feedback))))
           (review (if (equal nil review-comments)
                       partial-review
                     (a-assoc partial-review
                              'comments review-comments))))
      `((replies . ,replies)
        (review . ,review)))))

;;; Public APIs

;;;###autoload
(defun code-review-start (url)
  "Start review given PR URL."
  (interactive "sPR URL: ")
  (code-review-section-build-buffer
   (code-review-utils-pr-from-url url)))

;;;###autoload
(defun code-review-submit ()
  "Submit your review."
  (interactive)
  (let ((response (code-review-build-submit-structure)))
    (let-alist response
      (cond
       ((and (not .replies) (not .review.body))
        (message "Your review is empty."))

       ((not .review.body)
        (message "You need to provide a feedback message."))

       (t
        (progn
          (when .replies
            (code-review-github-post-replies
             code-review-pr-alist
             .replies
             (lambda (&rest _)
               (message "Done submitting review replies"))))
          (code-review-github-post-review
           code-review-pr-alist
           .review
           (lambda (&rest _)
             (message "Done submitting review"))))))
      (code-review-section-build-buffer code-review-pr-alist))))

;;;###autoload
(defun code-review-forge-pr-at-point ()
  "Review the forge pull request at point."
  (interactive)
  (let* ((pullreq (or (forge-pullreq-at-point) (forge-current-topic)))
         (repo    (forge-get-repository pullreq)))

    (if (not (forge-pullreq-p pullreq))
        (message "We can only review PRs at the moment. You tried on something else.")
      (progn
        (setq forge-current-dir default-directory)
        (code-review-section-build-buffer (a-alist 'owner   (oref repo owner)
                                                   'repo    (oref repo name)
                                                   'apihost (oref repo apihost)
                                                   'num     (oref pullreq number)))))))
;;;###autoload
(defun code-review-approve ()
  "Approve current PR."
  (interactive)
  (a-assoc code-review-pr-alist 'event "APPROVE")
  (code-review-submit))

;;;###autoload
(defun code-review-reject ()
  "Approve current PR."
  (interactive)
  (a-assoc code-review-pr-alist 'event "REJECT")
  (code-review-submit))

;;;###autoload
(defun code-review-request-changes ()
  "Approve current PR."
  (interactive)
  (a-assoc code-review-pr-alist 'event "REQUEST_CHANGE")
  (code-review-submit))

;;; transient

(transient-define-prefix code-review (review)
  "Approve, Reject, or Request changes to a Review."
  [("a" "Approve" code-review-approve)
   ("r" "Reject" code-review-reject)
   ("c" "Request Changes" code-review-request-changes)])

(transient-define-prefix code-review-comments (comment)
  "Add, Edit, Delete comments."
  [("a" "Add" code-review-comment-add)
   ("e" "Edit" code-review-comment-edit)
   ("d" "Delete" code-review-comment-delete)])

(define-transient-command code-review-transient-api ()
  "Code Review"
  ["Comment"
   ("c" "Comment" code-review-comments)]
  ["Review"
   ("a" "Add main comment" code-review-comment-add-feedback)
   ("e" "Edit main comment" code-review-comment-add)
   ("s" "Submit" code-review)]
  ["Fast track"
   ("l" "LGTM - Approved" code-review-add-comment)]
  ["Quit"
   ("q" "Quit" transient-quit-one)])

(defvar code-review-mode-map
  (let ((map (copy-keymap magit-section-mode-map)))
    (suppress-keymap map t)
    (define-key map (kbd "r") 'code-review-transient-api)
    (set-keymap-parent map magit-section-mode-map)
    map))

(define-derived-mode code-review-mode magit-section-mode "Code Review"
  "Code Review mode")

(provide 'code-review)
;;; code-review.el ends here
