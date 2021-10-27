;;; code-review-core.el --- Main APIs you need to provide to add a new forge -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(cl-defgeneric code-review-pullreq-infos (obj callback)
  "Return infos Pull Request from a OBJ running CALLBACK with results.")

(cl-defgeneric code-review-infos-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-pullreq-diff (obj callback)
  "Return diff for OBJ running CALLBACK with results.")

(cl-defgeneric code-review-diff-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-commit-diff (obj callback)
  "Return diff for OBJ running CALLBACK with results.")

(cl-defgeneric code-review-commit-comments (obj callback)
  "Return commit comments for OBJ running CALLBACK with results.")

(cl-defgeneric code-review-commit-comments-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-commit-diff-deferred (obj)
  "Run OBJ with deferred.")

(cl-defgeneric code-review-send-review (obj)
  "Send review stored in OBJ to the forge.")

(cl-defgeneric code-review-send-replies (obj)
  "Send review comment replies stored in OBJ to the forge.")

(cl-defgeneric code-review-get-labels (obj)
  "Sync call to get a list of labels from OBJ.")

(cl-defgeneric code-review-set-labels (obj)
  "Sync call to set a list of labels for an OBJ.")

(cl-defgeneric code-review-get-assignees (obj)
  "Sync call to get a list of assignees from OBJ.")

(cl-defgeneric code-review-set-assignee (obj)
  "Set an assignee for an OBJ.")

(cl-defgeneric code-review-get-milestones (obj)
  "Sync call to get a list of milestones from OBJ.")

(cl-defgeneric code-review-set-milestone (obj)
  "Set a milestone for an OBJ.")

(cl-defgeneric code-review-set-title (obj)
  "Set a pullrequest title for an OBJ.")

(cl-defgeneric code-review-set-description (obj)
  "Set a pullrequest description for an OBJ.")

(provide 'code-review-core)
;;; code-review-core.el ends here
