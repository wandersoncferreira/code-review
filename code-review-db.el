;;; code-review-db.el --- Manage code review database -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Wanderson Ferreira
;;
;; Author: Wanderson Ferreira <https://github.com/wandersoncferreira>
;; Maintainer: Wanderson Ferreira <wand@hey.com>
;; Version: 0.0.7
;; Homepage: https://github.com/wandersoncferreira/code-review
;;
;; This file is not part of GNU Emacs.
;;

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

(require 'a)
(require 'closql)
(require 'eieio)
(require 'uuidgen)
(require 'dash)

(defcustom code-review-db-database-file
  (expand-file-name "code-review-db-file.sqlite" user-emacs-directory)
  "The file used to store the `code-review' database."
  :group 'code-review
  :type 'file)

(defclass code-review-db-buffer (closql-object)
  ((closql-table        :initform 'buffer)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'pullreq)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (pullreq             :initarg :pullreq)
   (raw-text            :initform nil)
   (paths               :closql-class code-review-db-path)))

(defclass code-review-db-path (closql-object)
  ((closql-table        :initform 'path)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'buffer)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (name                :initarg :name)
   (head-pos            :initform nil)
   (buffer              :initarg :buffer)
   (at-pos-p            :initarg :at-pos-p)
   (comments            :closql-class code-review-db-comment)))

(defclass code-review-db-comment (closql-object)
  ((closql-table        :initform 'comment)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'path)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (path                :initarg :path)
   (loc-written         :initform nil)
   (identifiers         :initarg :identifiers)))

(defclass code-review-db-pullreq (closql-object)
  ((closql-table        :initform 'pullreq)
   (closql-primary-key  :initform 'id)
   (closql-class-prefix :initform "code-review-")
   (closql-order-by     :initform [(desc number)])
   (id                  :initarg :id)
   (base-ref-name       :initform nil)
   (head-ref-name       :initform nil)
   (finished            :initform nil)
   (finished-at         :initform nil)
   (saved               :initform nil)
   (saved-at            :initform nil)
   (raw-infos           :initform nil)
   (raw-diff            :initform nil)
   (raw-comments        :initform nil)
   (owner               :initarg :owner)
   (repo                :initarg :repo)
   (number              :initarg :number)
   (description         :initform nil)
   (title               :initform nil)
   (host                :initform nil)
   (sha                 :initform nil)
   (feedback            :initform nil)
   (state               :initform nil)
   (replies             :initform nil)
   (review              :initform nil)
   (labels              :initform nil)
   (merge               :initform nil)
   (milestones          :initform nil)
   (projects            :initform nil)
   (reviewers           :initform nil)
   (assignees           :initform nil)
   (linked-issues       :initform nil)
   (buffer              :closql-class code-review-db-buffer))
  :abstract t)

(defclass code-review-db-database (closql-database)
  ((name         :initform "code-review-db")
   (object-class :initform 'code-review-db-pullreq)
   (file         :initform 'code-review-db-database-file)
   (schemata     :initform 'code-review-db-table-schema)
   (version      :initform 8)))

(defvar code-review-db--override-connection-class nil)

(defvar code-review-db--sqlite-available-p t)

(defun code-review-db (&optional livep)
  (condition-case err
      (closql-db 'code-review-db-database livep code-review-db--override-connection-class)
    (error (setq code-review-db--sqlite-available-p nil)
           (signal (car err) (cdr err)))))

;;; Schema

(defconst code-review-db-table-schema
  '((pullreq
     [(class :not-null)
      (id :not-null :primary-key)
      base-ref-name
      head-ref-name
      finished
      finished-at
      saved
      saved-at
      raw-infos
      raw-diff
      raw-comments
      owner
      repo
      number
      description
      title
      host
      sha
      feedback
      state
      replies
      review
      labels
      merge
      milestones
      projects
      reviewers
      assignees
      linked-issues
      (buffer :default eieio-unbound)
      callback])

    (buffer
     [(class :not-null)
      (id :not-null :primary-key)
      pullreq
      raw-text
      (path :default eieio-unbound)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade))

    (path
     [(class :not-null)
      (id :not-null :primary-key)
      name
      head-pos
      buffer
      at-pos-p
      (comment :default eieio-unbound)]
     (:foreign-key
      [buffer] :references buffer [id]
      :on-delete :cascade))

    (comment
     [(class :not-null)
      (id :not-null :primary-key)
      path
      loc-written
      identifiers]
     (:foreign-key
      [path] :references path [id]
      :on-delete :cascade))))

(cl-defmethod closql--db-update-schema ((db code-review-db-database))
  (let ((code-version (oref-default 'code-review-db-database version))
        (version (closql--db-get-version db)))
    (closql-with-transaction db
      (when (= version 7)
        (message "Upgrading Code Review database from version 7 to 8...")
        (emacsql db [:alter-table pullreq :add-column base-ref-name :default nil])
        (emacsql db [:alter-table pullreq :add-column head-ref-name :default nil])
        (closql--db-set-version db (setq version 8))
        (message "Upgrading Code Review database from version 7 to 8...done"))
      (cl-call-next-method))))

;;; Core

(defvar code-review-db--pullreq-id nil)

;; Helper

(defun code-review-db-update (obj)
  "Update whole OBJ in datatabase."
  (closql-insert (code-review-db) obj t))

(defun code-review-db-search (owner repo number)
  "Find eieio obj of PR for OWNER, REPO, and NUMBER."
  (let ((db (code-review-db))
        (class 'code-review-db-pullreq))
    (->> (emacsql db [:select :*
                      :from 'pullreq
                      :where (and (= owner $s1)
                                  (= repo $s2)
                                  (= number $s3)
                                  (= saved 't)
                                  (is finished nil))]
                  owner
                  repo
                  number)
         (mapcar
          (lambda (row) (closql--remake-instance class db row)))
         (-last-item))))

(defun code-review-db-all-unfinished ()
  "Get a list of all unfinished Reviews."
  (when (and code-review-db-connection (emacsql-live-p code-review-db-connection))
    (emacsql-close code-review-db-connection)
    (setq code-review-db-connection nil))
  (let ((class 'code-review-db-pullreq)
        (db (code-review-db)))
    (->> (emacsql db
                  [:select :*
                   :from 'pullreq
                   :where (and (= saved 't)
                               (is finished nil))])
         (mapcar
          (lambda (row) (closql--remake-instance class db row))))))

;;; Domain

;; Simplified getters

(defun code-review-db-get-pullreq ()
  "Get pullreq obj from ID."
  (closql-get (code-review-db) code-review-db--pullreq-id 'code-review-db-pullreq))

(defun code-review-db-get-buffer ()
  "Get buffer obj from BUFFER-ID."
  (closql-get (code-review-db) code-review-db--pullreq-id 'code-review-db-buffer))

(defun code-review-db-get-path ()
  "Get path obj from ID."
  (closql-get (code-review-db) code-review-db--pullreq-id 'code-review-db-path))

(defun code-review-db-get-buffer-paths ()
  "Get paths from BUFFER-ID."
  (let* ((buffer (code-review-db-get-buffer)))
    (oref buffer paths)))

(defun code-review-db-get-comment (id)
  "Get comment obj from ID."
  (closql-get (code-review-db) id 'code-review-db-comment))

(defun code-review-db-get-curr-head-pos ()
  "Get the head-pos value for the current path in the pullreq."
  (let ((path (code-review-db--curr-path)))
    (oref path head-pos)))

;; ...

(defun code-review-db--pullreq-create (obj)
  "Create a pullreq db object from OBJ."
  (let* ((pr-id (uuidgen-4)))
    (oset obj id pr-id)
    (closql-insert (code-review-db) obj t)
    (setq code-review-db--pullreq-id pr-id)))

(defun code-review-db-get-pr-alist ()
  "Get pr-alist from ID."
  (let ((pr (code-review-db-get-pullreq)))
    (a-alist 'num (oref pr number)
             'owner (oref pr owner)
             'repo (oref pr repo)
             'sha (oref pr sha))))

(defun code-review-db--pullreq-sha-update (sha-value)
  "Update pullreq obj of ID with value SHA-VALUE."
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr sha sha-value)
    (closql-insert (code-review-db) pr t)))

(defun code-review-db--pullreq-raw-infos-update (infos)
  "Save INFOS to the PULLREQ entity."
  (let ((pullreq (code-review-db-get-pullreq)))
    (let-alist infos
      (oset pullreq raw-infos infos)
      (oset pullreq title .title)
      (oset pullreq state .state)
      (oset pullreq base-ref-name .baseRefName)
      (oset pullreq head-ref-name .headRefName)
      (oset pullreq description .bodyHTML)
      (oset pullreq sha .headRefOid)
      (oset pullreq raw-comments .reviews.nodes)
      (oset pullreq assignees .assignees.nodes)
      (oset pullreq milestones `((title . ,.milestone.title)
                                 (perc . ,.milestone.progressPercentage)
                                 (number . nil)))
      (closql-insert (code-review-db) pullreq t))))

(defun code-review-db--pullreq-raw-diff-update (raw-diff)
  "Save RAW-DIFF to the PULLREQ entity."
  (let ((pullreq (code-review-db-get-pullreq)))
    (oset pullreq raw-diff raw-diff)
    (closql-insert (code-review-db) pullreq t)))

(defun code-review-db--pullreq-raw-infos ()
  "Get raw infos alist from ID."
  (oref (code-review-db-get-pullreq) raw-infos))

(defun code-review-db--pullreq-raw-comments ()
  "Get raw comments alist from ID."
  (oref (code-review-db-get-pullreq) raw-comments))

(defun code-review-db--pullreq-raw-diff ()
  "Get raw diff alist from ID."
  (oref (code-review-db-get-pullreq) raw-diff))

(defun code-review-db--pullreq-title ()
  "Get title of pullreq."
  (oref (code-review-db-get-pullreq) title))

(defun code-review-db--pullreq-state ()
  "Get state of pullreq."
  (oref (code-review-db-get-pullreq) state))

(defun code-review-db--pullreq-title-update (pr title)
  "Update the PR's TITLE."
  (oset pr title title)
  (closql-insert (code-review-db) pr t))

(defun code-review-db--pullreq-description ()
  "Get description of pullreq."
  (oref (code-review-db-get-pullreq) description))

(defun code-review-db--pullreq-labels ()
  "Get labels of pullreq."
  (oref (code-review-db-get-pullreq) labels))

(defun code-review-db--pullreq-assignees ()
  "Get assignees of pullreq."
  (oref (code-review-db-get-pullreq) assignees))

(defun code-review-db--pullreq-milestones ()
  "Get milestones of pullreq."
  (oref (code-review-db-get-pullreq) milestones))

(defun code-review-db--pullreq-description-update (pr description)
  "Update the PR's DESCRIPTION."
  (oset pr description description)
  (closql-insert (code-review-db) pr t))

(defun code-review-db--pullreq-raw-comments-update (comment)
  "Add COMMENT to the pullreq ID."
  (let* ((pr (code-review-db-get-pullreq))
         (raw-comments (oref pr raw-comments)))
    (oset pr raw-comments (append raw-comments (list comment)))
    (code-review-db-update pr)))

(defun code-review-db--pullreq-feedback ()
  "Get feedback for the current pr."
  (let ((pr (code-review-db-get-pullreq)))
    (oref pr feedback)))

(defun code-review-db--pullreq-feedback-update (feedback)
  "Save most recent FEEDBACK."
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr feedback feedback)
    (closql-insert (code-review-db) pr t)))

;;;

(defun code-review-db--curr-path-update (curr-path)
  "Update pullreq (ID) with CURR-PATH."
  (let* ((buf (code-review-db-get-buffer))
         (new-path-id (uuidgen-4))
         (db (code-review-db)))
    (if (not buf)
        (let* ((pr (code-review-db-get-pullreq))
               (pr-id (oref pr id))
               (buf (code-review-db-buffer :id pr-id :pullreq pr-id))
               (path (code-review-db-path :id new-path-id
                                          :buffer pr-id
                                          :name curr-path
                                          :at-pos-p t)))
          (closql-with-transaction db
            (closql-insert db buf t)
            (closql-insert db path t)))
      (let* ((paths (oref buf paths))
             (curr-path-re-enabled? nil))
        (closql-with-transaction db
          ;;; disable all previous ones and enable curr-path is already exists
          (-map
           (lambda (path)
             (if (string-equal (oref path name) curr-path)
                 (progn
                   (oset path at-pos-p t)
                   (closql-insert db path t)
                   (setq curr-path-re-enabled? t))
               (progn
                 (oset path at-pos-p nil)
                 (closql-insert db path t))))
           paths)
          (when (not curr-path-re-enabled?)
            ;; save new one
            (closql-insert db (code-review-db-path
                               :id new-path-id
                               :buffer (oref buf id)
                               :name curr-path
                               :at-pos-p t)
                           t)))))))

(defun code-review-db--curr-path-head-pos-update (curr-path hunk-head-pos)
  "Update pullreq (ID) on CURR-PATH using HUNK-HEAD-POS."
  (let* ((buf (code-review-db-get-buffer))
         (paths (oref buf paths)))
    (dolist (p paths)
      (when (string-equal (oref p name) curr-path)
        (oset p head-pos hunk-head-pos)
        (closql-insert (code-review-db) p t)))))

(defun code-review-db--head-pos (path)
  "Get the first hunk position given a ID and PATH."
  (let* ((pr (code-review-db-get-pullreq))
         (buff (oref pr buffer))
         (buf (if (eieio-object-p buff) buff (-first-item buff)))
         (paths (oref buf paths))
         (res
          (->> paths
               (-filter
                (lambda (p)
                  (string-equal (oref p name) path)))
               (-first-item))))
    (oref res head-pos)))

(defun code-review-db--curr-path-comment-count-update (count)
  "Update pullreq (ID) on CURR-PATH using COUNT."
  (let* ((comment (code-review-db--curr-comment)))
    (oset comment loc-written (+ (or (oref comment loc-written) 0) count))
    (closql-insert (code-review-db) comment t)))


;;; Accessor Functions

(defun code-review-db--curr-path ()
  "Get the latest activated path for the current pullreq obj."
  (let* ((buf (code-review-db-get-buffer)))
    (->> (oref buf paths)
         (-filter (lambda (p) (oref p at-pos-p)))
         (-first-item))))

(defun code-review-db--curr-comment ()
  "Get the latest activated path comment for the current pullreq obj."
  (let* ((path (code-review-db--curr-path)))
    (code-review-db-get-comment (oref path id))))

(defun code-review-db--curr-path-name ()
  "Get the latest activated patch for the current pullreq obj ID."
  (let* ((path (code-review-db--curr-path)))
    (oref path name)))

;;;

(defun code-review-db--curr-path-comment-written-update (identifier)
  "Update pullreq (ID) on curr path using IDENTIFIER."
  (if-let (curr-comment (code-review-db--curr-comment))
      (progn
        (oset curr-comment identifiers
              (cons identifier
                    (oref curr-comment identifiers)))
        (closql-insert (code-review-db) curr-comment t))
    (let* ((path (code-review-db--curr-path))
           (c (code-review-db-comment :id (oref path id)
                                      :path (oref path id)
                                      :identifiers (list identifier))))
      (closql-insert (code-review-db) c t))))

;; comments

(defun code-review-db--comment-already-written? (identifier)
  "Verify if comment with IDENTIFIER was marked as written.
Very Bad Performance!."
  (let* ((buffer (code-review-db-get-buffer))
         (paths (oref buffer paths)))
    (-reduce-from
     (lambda (written? path)
       (let* ((comments (oref path comments))
              (comment (if (eieio-object-p comments) comments (-first-item comments))))
         (if written?
             t
           (when comment
             (and (-contains-p (oref comment identifiers) identifier) t)))))
     nil
     paths)))

(defun code-review-db-get-comment-written-pos ()
  "Get loc-written value for comment ID."
  (let ((comment (code-review-db--curr-comment)))
    (if (not comment)
        0
      (oref comment loc-written))))

(defun code-review-db-delete-raw-comment (internal-id)
  "Remove INTERNAL-ID comment from raw comments list."
  (let* ((pr (code-review-db-get-pullreq))
         (new-comments
          (-filter
           (lambda (c)
             (let ((res (-filter
                         (lambda (node)
                           (not (string-equal (a-get node 'internal-id) internal-id)))
                         (a-get-in c (list 'comments 'nodes)))))
               res))
           (oref pr raw-comments))))
    (oset pr raw-comments new-comments)
    (closql-insert (code-review-db) pr t)))

(provide 'code-review-db)
;;; code-review-db.el ends here
