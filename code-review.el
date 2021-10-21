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

(require 'closql)
(require 'magit-section)
(require 'code-review-section)
(require 'code-review-github)
(require 'code-review-comment)
(require 'code-review-utils)
(require 'code-review-db)
(require 'code-review-core)

(defconst code-review-buffer-name "*Code Review*")

(defvar code-review-pullreq-id nil)

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


(defun code-review-commit-at-point ()
  "Review the current commit at point in Code Review buffer."
  (interactive)
  (code-review-section--build-commit-buffer
   (code-review-github-repo :sha "f30fb1e28766c043ce1876d499b7ec6b908bb651"
                            :owner "charignon"
                            :repo "github-review")))

;;;###autoload
(defun code-review-approve ()
  "Approve current PR."
  (interactive)
  (code-review-submit "APPROVE"))

;;;###autoload
(defun code-review-reject ()
  "Approve current PR."
  (interactive)
  (code-review-submit "REJECT"))

;;;###autoload
(defun code-review-request-changes ()
  "Approve current PR."
  (interactive)
  (code-review-submit "REQUEST_CHANGE"))

;;;###autoload
(defun code-review-submit (event)
  "Submit your review with a final veredict (EVENT)."
  (interactive)
  (let ((obj (code-review-utils--gen-submit-structure
              code-review-pullreq-id)))
    (oset obj event event)
    (cond
     ((and (not (oref obj replies)) (a-get (oref obj review) 'body))
      (message "Your review is empty."))

     ((not (a-get (oref obj review) 'body))
      (message "You need to provide a feedback message."))

     (t
      (progn
        (when (oref obj replies)
          (code-review-send-replies
           obj
           (lambda (&rest _)
             (message "Done submitting review replies"))))
        (code-review-send-review
         obj
         (lambda (&rest _)
           (message "Done submitting review"))))))
    (code-review-section--build-buffer obj)))

;;; Entrypoint


;;;###autoload
(defun code-review-start (url)
  "Start review given PR URL."
  (interactive "sPR URL: ")
  (let ((obj (code-review-utils-build-obj-from-url url)))
    (code-review-section--build-buffer obj)))

;;;###autoload
(defun code-review-forge-pr-at-point ()
  "Review the forge pull request at point."
  (interactive)
  (let* ((pullreq (or (forge-pullreq-at-point) (forge-current-topic)))
         (repo    (forge-get-repository pullreq)))

    (if (not (forge-pullreq-p pullreq))
        (message "We can only review PRs at the moment. You tried on something else.")
      (let* ((pr-alist (a-alist 'owner   (oref repo owner)
                                'repo    (oref repo name)
                                'apihost (oref repo apihost)
                                'num     (oref pullreq number)))
             (obj (code-review-utils-build-obj pr-alist)))
        (code-review-section--build-buffer obj)))))

;;; Transient

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
   ("l" "LGTM - Approved" code-review-comment-add)]
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
