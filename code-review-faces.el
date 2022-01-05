;;; code-review-faces.el --- Define faces used in the package -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.6
;; Homepage: https://github.com/wandersoncferreira/code-review-faces
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
;;  Define faces used in the package
;;
;;; Code:

(defface code-review-timestamp-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DimGrey"
     :slant italic)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DimGrey"
     :slant italic))
  "Face for timestamps."
  :group 'code-review)

(defface code-review-comment-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DimGrey")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "LightGrey"))
  "Face for comment sections."
  :group 'code-review)

(defface code-review-thread-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "SlateGrey"
     :weight bold)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "SlateGrey"
     :weight bold))
  "Face for threads."
  :group 'code-review)

(defface code-review-state-face
  '((t :inherit bold))
  "Face used for default state keywords."
  :group 'code-review)

(defface code-review-checker-name-face
  '((t :inherit bold :slant italic))
  "Face used for commit check name."
  :group 'code-review)

(defface code-review-checker-detail-face
  '((t :inherit magit-section-heading
       :slant italic
       :weight normal))
  "Face for details word."
  :group 'code-review)

(defface code-review-author-face
  '((t :inherit font-lock-keyword-face))
  "Face used for author names."
  :group 'code-review)

(defface code-review-author-header-face
  '((t :inherit font-lock-keyword-face
       :slant italic
       :underline t))
  "Face used for author name in the header."
  :group 'code-review)

(defface code-review-error-state-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face used for error state (e.g. changes requested)."
  :group 'code-review)

(defface code-review-success-state-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "Green")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "Green"))
  "Face used for success state (e.g. merged)."
  :group 'code-review)

(defface code-review-info-state-face
  '((t :slant italic))
  "Face used for info (unimportant) state (e.g. resolved)."
  :group 'code-review)

(defface code-review-pending-state-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "yellow4")
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkKhaki"))
  "Face used for pending state."
  :group 'code-review)

(defface code-review-request-review-face
  `((((class color) (background light))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "yellow4"
     :slant italic
     :underline t)
    (((class color) (background  dark))
     ,@(and (>= emacs-major-version 27) '(:extend t))
     :foreground "DarkKhaki"
     :slant italic
     :underline t))
  "Face used for pending state."
  :group 'code-review)

(defface code-review-recent-comment-heading
  '((((supports (:box t))) :inherit magit-branch-remote :box t)
    (t                     :inherit magit-branch-remote :inverse-video t))
  "Face for recent comments."
  :group 'code-review)

(defface code-review-outdated-comment-heading
  '((((supports (:box t))) :inherit magit-cherry-equivalent :box t)
    (t                     :inherit magit-cherry-equivalent :inverse-video t))
  "Face for outdated comments."
  :group 'code-review)

(defface code-review-dimmed
  '((((class color) (background light))
     :foreground "grey50"
     :slant italic
     :underline t)
    (((class color) (background  dark))
     :foreground "grey50"
     :slant italic
     :underline t))
  "Face for text that shouldn't stand out."
  :group 'magit-faces)

(provide 'code-review-faces)
;;; code-review-faces.el ends here
