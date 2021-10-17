;;; code-review.el --- Perform code review from Github -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/wandersoncferreira/code-review
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'magit-section)
(require 'code-review-section)
(require 'code-review-comment)
(require 'code-review-utils)

(defconst code-review-buffer-name "*Code Review*")

(defvar code-review-pr-alist nil
  "For internal usage only.")

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
                       (path-pos (code-review-utils-path-pos-key .path handled-pos)))
                  (if (or (not grouped-comments)
                          (not (code-review-utils-get-comments grouped-comments path-pos)))
                      (a-assoc grouped-comments path-pos (list comment-enriched))
                    (a-update grouped-comments path-pos (lambda (v) (append v (list comment-enriched))))))))
            acc
            comments)
         acc)))
   nil
   (a-get-in pull-request (list 'reviews 'nodes))))

(defun code-review-section-build-buffer (pr-alist)
  "Build code review buffer given a PR-ALIST with basic info about target repo."
  (setq code-review-pr-alist pr-alist)
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
                      (concat (propertize " " 'display '(space :align-to 0))
                              (format "#%s: %s"
                                      (a-get pull-request 'number)
                                      (a-get pull-request 'title))))
                (code-review-section-insert-headers pull-request)
                (code-review-section-insert-commits)
                (magit-wash-sequence
                 (apply-partially #'code-review-section-wash grouped-comments)))
              (goto-char (point-min)))))))
    (deferred:error it
      (lambda (err)
        (message "Got an error from your VC provider %S!" err)))))

(defun code-review-pr-from-url (url)
  "Extract a pr alist from a pull request URL."
  (save-match-data
    (and (string-match ".*/\\(.*\\)/\\(.*\\)/pull/\\([0-9]+\\)" url)
         (a-alist 'num   (match-string 3 url)
                  'repo  (match-string 2 url)
                  'owner (match-string 1 url)))))

(defun code-review-approve ()
  "Approve current PR."
  (a-assoc code-review-pr-alist 'code-review-state "APPROVE"))

(defun code-review-reject ()
  "Approve current PR."
  (a-assoc code-review-pr-alist 'code-review-state "REJECT"))

(defun code-review-request-changes ()
  "Approve current PR."
  (a-assoc code-review-pr-alist 'code-review-state "REQUEST_CHANGE"))

;;; Public APIs

;;;###autoload
(defun code-review-start (url)
  "Start review given PR URL."
  (interactive "sPR URL: ")
  (setq code-review-section-first-hunk-header-pos nil
        code-review-section-written-comments-count nil
        code-review-section-written-comments-ident nil)
  (code-review-section-build-buffer
   (code-review-pr-from-url url)))

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
    (let* ((partial-review `((commit_id . (a-get code-review-pr-alist 'sha))
                             (body . "Default msg")))
           (review (if (equal nil review-comments)
                       partial-review
                     (a-assoc partial-review 'comments review-comments))))
      `((replies . ,replies)
        (review . ,review)))))

;;;###autoload
(defun code-review-submit ()
  "Submit your review."
  (interactive)
  (let ((state (completing-read "Your veredict about the PR: "
                                (list "APPROVE"
                                      "REQUEST_CHANGES"
                                      "COMMENT"))))
    ;; (let ((response (code-review-build-submit-structure)))
    ;;   (let-alist response
    ;;     (if (and (not .replies) (not .review))
    ;;         (message "Your review is empty")
    ;;       (progn
    ;;         (when .replies
    ;;           (code-review-github-post-replies
    ;;            code-review-pr-alist
    ;;            .replies
    ;;            (lambda (&rest _)
    ;;              (message "Done submitting review replies"))))
    ;;         (when .review
    ;;           (code-review-github-post-review
    ;;            code-review-pr-alist
    ;;            (a-assoc .review 'event state)
    ;;            (lambda (&rest _)
    ;;              (message "Done submitting review"))))))))
    ))

;;; transient

(transient-define-prefix code-review (review)
  "Approve, Reject, or Request changes to a Review."
  [("a" "Approve" code-review-approve)
   ("r" "Reject" code-review-reject)
   ("c" "Request Changes" code-review-request-changes)])

(define-transient-command code-review-transient-api ()
  "Code Review"
  ["Comments"
   ("a" "Add" code-review-comment-add)
   ("e" "Edit" code-review-comment-edit)
   ("d" "Delete" code-review-comment-delete)]
  ["Review"
   ("a" "Add main comment" code-review-comment)
   ("e" "Edit main comment" code-review-comment)
   ("s" "Submit" code-review)]
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
