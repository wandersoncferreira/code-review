;;; code-review-db.el --- Manage code review database -*- lexical-binding: t; -*-
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

(require 'closql)
(require 'eieio)
(require 'uuidgen)

(defcustom code-review-database-connector 'sqlite
  "The database connector."
  :group 'code-review)

(defcustom code-review-database-file
  (expand-file-name "code-review-database.sqlite" user-emacs-directory)
  "The file used to store the `code-review' database."
  :group 'code-review
  :type 'file)

(declare-function code-review-database--eieio-childp "code-review-db.el" (obj) t)

(defclass code-review-buffer (closql-object)
  ((closql-table        :initform 'buffer)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'pullreq)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (pullreq             :initarg :pullreq)
   (raw-text            :initform nil)
   (paths               :closql-class code-review-path)))

(defclass code-review-path (closql-object)
  ((closql-table        :initform 'path)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'buffer)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (name                :initarg :name)
   (head-pos            :initform nil)
   (buffer              :initarg :buffer)
   (at-pos-p            :initarg :at-pos-p)
   (comments            :closql-class code-review-comment)))

(defclass code-review-comment (closql-object)
  ((closql-table        :initform 'comment)
   (closql-primary-key  :initform 'id)
   (closql-foreign-key  :initform 'path)
   (closql-class-prefix :initform "code-review-")
   (id                  :initarg :id)
   (path                :initarg :path)
   (loc-written         :initform nil)
   (identifiers         :initarg :identifiers)))

(defclass code-review-pullreq (closql-object)
  ((closql-table        :initform 'pullreq)
   (closql-primary-key  :initform 'id)
   (closql-class-prefix :initform "code-review-")
   (closql-order-by     :initform [(desc number)])
   (id                  :initarg :id)
   (raw-infos           :initform nil)
   (raw-diff            :initform nil)
   (raw-comments        :initform nil)
   (owner               :initarg :owner)
   (repo                :initarg :repo)
   (number              :initarg :number)
   (host                :initform nil)
   (sha                 :initform nil)
   (feedback            :initform nil)
   (state               :initform nil)
   (replies             :initform nil)
   (review              :initform nil)
   (buffer              :closql-class code-review-buffer))
  :abstract t)

(defclass code-review-database (emacsql-sqlite-connection closql-database)
  ((object-class :initform 'code-review-pullreq)))

(defconst code-review--db-version 7)

(defconst code-review-db--sqlite-available-p
  (with-demoted-errors "Code Review initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t))

;; (setq code-review--db-connection nil)
(defvar code-review--db-connection nil
  "The EmacSQL database connection.")

(defun code-review-db ()
  "Start connection."
  (unless (and code-review--db-connection (emacsql-live-p code-review--db-connection))
    (make-directory (file-name-directory code-review-database-file) t)
    (closql-db 'code-review-database 'code-review--db-connection
               code-review-database-file t))
  code-review--db-connection)

;;; Schema

(defconst code-review--db-table-schema
  '((pullreq
     [(class :not-null)
      (id :not-null :primary-key)
      raw-infos
      raw-diff
      raw-comments
      host
      sha
      owner
      repo
      number
      feedback
      replies
      review
      state
      callback
      (buffer :default eieio-unbound)])

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

(cl-defmethod closql--db-init ((db code-review-database))
  "Initialize the DB."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) code-review--db-table-schema)
      (emacsql db [:create-table $i1 $S2] table schema))
    (closql--db-set-version db code-review--db-version)))

;;; Core

(defvar pullreq-id nil)
;; (put 'pullreq-id 'permanent-local t)

;; Helper

(defun code-review-db-update (obj)
  "Update whole OBJ in datatabase."
  (closql-insert (code-review-db) obj t))

;;; Domain

;; Simplified getters

(defun code-review-db-get-pullreq ()
  "Get pullreq obj from ID."
  (closql-get (code-review-db) pullreq-id 'code-review-github-repo))

(defun code-review-db-get-buffer ()
  "Get buffer obj from BUFFER-ID."
  (closql-get (code-review-db) pullreq-id 'code-review-buffer))

(defun code-review-db-get-path ()
  "Get path obj from ID."
  (closql-get (code-review-db) pullreq-id 'code-review-path))

(defun code-review-db-get-buffer-paths ()
  "Get paths from BUFFER-ID."
  (let* ((buffer (code-review-db-get-buffer)))
    (oref buffer paths)))

(defun code-review-db-get-comment (id)
  "Get comment obj from ID."
  (closql-get (code-review-db) id 'code-review-comment))

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
    (setq pullreq-id pr-id)))

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
    (oset pullreq raw-infos infos)
    (oset pullreq sha (a-get-in infos (list 'headRef 'target 'oid)))
    (oset pullreq raw-comments (a-get-in infos (list 'reviews 'nodes)))
    (closql-insert (code-review-db) pullreq t)))

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

(defun code-review-db--pullreq-raw-comments-update (comment)
  "Add COMMENT to the pullreq ID."
  (let* ((pr (code-review-db-get-pullreq))
         (raw-comments (oref pr raw-comments)))
    (oset pr raw-comments (append raw-comments (list comment)))
    (closql-insert (code-review-db) pr t)))

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
               (buf (code-review-buffer :id pr-id :pullreq pr-id))
               (path (code-review-path :id new-path-id
                                       :buffer pr-id
                                       :name curr-path
                                       :at-pos-p t)))
          (emacsql-with-transaction db
            (closql-insert db buf t)
            (closql-insert db path t)))
      (let* ((paths (oref buf paths))
             (curr-path-re-enabled? nil))
        (emacsql-with-transaction db
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
            (closql-insert db (code-review-path
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
           (c (code-review-comment :id (oref path id)
                                   :path (oref path id)
                                   :identifiers (list identifier))))
      (closql-insert (code-review-db) c t))))

;; comments

(defun code-review-db--comment-already-written? (identifier)
  "Verify if comment from pullreq BUFFER-ID with IDENTIFIER was already marked as written."
  (let* ((buffer (code-review-db-get-buffer))
         (paths (oref buffer paths)))
    (-reduce-from
     (lambda (written? path)
       (let* ((comments (oref path comments))
              (comment (if (eieio-object-p comments) comments (-first-item comments))))
         (if written?
             written?
           (when comment
             (-contains-p (oref comment identifiers) identifier)))))
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
