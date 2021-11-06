;;; code-review-core.el --- Main APIs you need to provide to add a new forge -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.1
;; Homepage: https://github.com/wandersoncferreira/code-review
;; Package-Requires: ((emacs "25.1"))
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
;;  Description
;;
;;; Code:

(cl-defgeneric code-review-core-pullreq-infos (obj callback)
  "Return infos Pull Request from a OBJ running CALLBACK with results.")

(cl-defgeneric code-review-core-infos-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-core-pullreq-diff (obj callback)
  "Return diff for OBJ running CALLBACK with results.")

(cl-defgeneric code-review-core-diff-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-core-commit-diff (obj callback)
  "Return diff for OBJ running CALLBACK with results.")

(cl-defgeneric code-review-core-commit-comments (obj callback)
  "Return commit comments for OBJ running CALLBACK with results.")

(cl-defgeneric code-review-core-commit-comments-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-core-commit-diff-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-core-send-review (obj callback)
  "Send review stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-core-send-comments (obj callback)
  "Send comments stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-core-send-replies (obj callback)
  "Send review comment replies stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-core-get-labels (obj)
  "Sync call to get a list of labels from OBJ.")

(cl-defgeneric code-review-core-set-labels (obj)
  "Sync call to set a list of labels for an OBJ.")

(cl-defgeneric code-review-core-get-assignees (obj)
  "Sync call to get a list of assignees from OBJ.")

(cl-defgeneric code-review-core-set-assignee (obj)
  "Set an assignee for an OBJ.")

(cl-defgeneric code-review-core-get-milestones (obj)
  "Sync call to get a list of milestones from OBJ.")

(cl-defgeneric code-review-core-set-milestone (obj)
  "Set a milestone for an OBJ.")

(cl-defgeneric code-review-core-set-title (obj)
  "Set a pullrequest title for an OBJ.")

(cl-defgeneric code-review-core-set-description (obj)
  "Set a pullrequest description for an OBJ.")

(cl-defgeneric code-review-core-merge (obj strategy)
  "Merge a PR for an OBJ using a given STRATEGY.")

(provide 'code-review-core)
;;; code-review-core.el ends here
