;;; code-review-section-header.el --- Functions to section headers -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.5
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
;;  Functions to section headers
;;
;;; Code:

(require 'code-review-actions)

;;; * Header line

(defun code-review-section-header-insert-headerline ()
  "Insert the title header line."
  (let ((pr (code-review-db-get-pullreq)))
    (setq header-line-format
          (propertize
           (format "#%s: %s" (oref pr number) (oref pr title))
           'font-lock-face
           'magit-section-heading))))

;;; * Title

(defclass code-review-title-section (magit-section)
  ((keymap  :initform 'code-review-title-section-map)
   (title  :initarg :title
           :type string)))

(defvar code-review-title-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-set-title)
    map)
  "Keymaps for code-comment sections.")

(defun code-review-section-header-insert-title ()
  "Insert the title of the header buffer."
  (let* ((title (code-review-db--pullreq-title))
         (obj (code-review-title-section :title title)))
    (magit-insert-section (code-review-title-section obj)
      (insert (format "%-17s" "Title: ") title)
      (insert ?\n))))

;;; * State

(defclass code-review-state-section (magit-section)
  ((state  :initform nil
           :type (or null string))))

(defun code-review-section-header-insert-state ()
  "Insert the state of the header buffer."
  (when-let (state (code-review-db--pullreq-state))
    (let ((value (if state state "none")))
      (magit-insert-section (code-review-state-section value)
        (insert (format "%-17s" "State: ") value)
        (insert ?\n)))))


;;; * Branch names

(defclass code-review-ref-section (magit-section)
  ((base   :initarg :base
           :type string)
   (head   :initarg :head
           :type string)))

(defun code-review-section-header-insert-ref ()
  "Insert the state of the header buffer."
  (let* ((pr (code-review-db-get-pullreq))
         (obj (code-review-ref-section
               :base (oref pr base-ref-name)
               :head (oref pr head-ref-name))))
    (magit-insert-section (code-review-ref-section obj)
      (insert (format "%-17s" "Refs: "))
      (insert (oref pr base-ref-name))
      (insert (propertize " ... " 'font-lock-face 'magit-dimmed))
      (insert (oref pr head-ref-name))
      (insert ?\n))))


;;; * Milestone

(defclass code-review-milestone-section (magit-section)
  ((keymap :initform 'code-review-milestone-section-map)
   (title  :initarg :title)
   (perc   :initarg :perc)
   (number :initarg :number
           :type number)))

(defvar code-review-milestone-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'code-review-set-milestone)
    map)
  "Keymaps for milestone section.")

(cl-defmethod code-review-pretty-milestone ((obj code-review-milestone-section))
  "Get the pretty version of milestone for a given OBJ."
  (cond
   ((and (oref obj title) (oref obj perc))
    (format "%s (%s%%)"
            (oref obj title)
            (oref obj perc)))
   ((oref obj title)
    (oref obj title))
   (t
    "No milestone")))

(defun code-review-section-header-insert-milestone ()
  "Insert the milestone of the header buffer."
  (let-alist (code-review-db--pullreq-milestones)
    (let* ((title (when (not (string-empty-p .title)) .title))
           (obj (code-review-milestone-section :title title :perc .perc)))
      (magit-insert-section (code-review-milestone-section obj)
        (insert (format "%-17s" "Milestone: "))
        (insert (propertize (code-review-pretty-milestone obj) 'font-lock-face 'magit-dimmed))
        (insert ?\n)))))

(provide 'code-review-section-header)
;;; code-review-section-header.el ends here
