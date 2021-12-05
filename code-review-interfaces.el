;;; code-review-interfaces.el --- Main APIs you need to provide to add a new forge -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.1
;; Homepage: https://github.com/wandersoncferreira/code-review
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

(cl-defgeneric code-review-send-review (obj callback)
  "Send review stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-send-comments (obj callback)
  "Send comments stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-send-replies (obj callback)
  "Send review comment replies stored in OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-get-labels (obj)
  "Sync call to get a list of labels from OBJ.")

(cl-defgeneric code-review-set-labels (obj callback)
  "Sync call to set a list of labels for an OBJ and call CALLBACK afterward..")

(cl-defgeneric code-review-get-assignees (obj)
  "Sync call to get a list of assignees from OBJ.")

(cl-defgeneric code-review-set-assignee (obj callback)
  "Set an assignee for an OBJ and call CALLBACK afterward..")

(cl-defgeneric code-review-get-milestones (obj)
  "Sync call to get a list of milestones from OBJ.")

(cl-defgeneric code-review-set-milestone (obj callback)
  "Set a milestone for an OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-set-title (obj callback)
  "Set a pullrequest title for an OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-set-description (obj callback)
  "Set a pullrequest description for an OBJ and call CALLBACK afterward.")

(cl-defgeneric code-review-merge (obj strategy)
  "Merge a PR for an OBJ using a given STRATEGY.")

(cl-defgeneric code-review-set-reaction (obj context-name comment-id reaction)
  "Set a REACTION to a COMMENT-ID in OBJ given a CONTEXT-NAME.")

(cl-defgeneric code-review-delete-reaction (obj context-name comment-id reaction-id)
  "Delete a REACTION to a COMMENT-ID in OBJ given a CONTEXT-NAME.")

(cl-defgeneric code-review-get-assinable-users (obj)
  "Get users that can review a PR for OBJ.")

(cl-defgeneric code-review-request-review (obj user-ids callback)
  "Request for OBJ a list of USER-IDS to review a PR and call CALLBACK afterward.")

(cl-defgeneric code-review-new-issue (obj body title callback)
  "Create new issue in OBJ with BODY and TITLE and call CALLBACK.")

(cl-defgeneric code-review-new-issue-comment (obj comment-msg callback)
  "Create a new comment issue for OBJ sending the COMMENT-MSG and call CALLBACK.")

(cl-defgeneric code-review-commit-checks (obj commit-sha)
  "Verify all the check for in the OBJ for a given COMMIT-SHA.")

;; * Rendering functions

(cl-defgeneric code-review--insert-general-comment-section (obj)
  "Render the general comment section for an OBJ.")

(cl-defgeneric code-review-render--auth-token-set? (obj forge-response)
  "Given an OBJ determine if the auth token was set by the FORGE-RESPONSE.")

(cl-defgeneric code-review-render--internal-build (obj progress-report forge-response &optional buff-name msg)
  "Given an OBJ update the PROGRESS-REPORT for the parsing FORGE-RESPONSE data.
Optionally provides the BUFF-NAME that must be rendered and a final MSG.")

(provide 'code-review-interfaces)
;;; code-review-interfaces.el ends here
