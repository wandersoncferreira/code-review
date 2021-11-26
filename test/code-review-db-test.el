;;; code-review-db-test.el --- Test our utility functions
;;; Commentary:
;;; Code:

(require 'a)
(require 'dash)
(require 'uuidgen)
(require 'buttercup)
(require 'code-review-db)
(require 'code-review-gitlab)
(require 'code-review-github)

(defconst sample-pr-alist
  (a-alist 'owner "owner"
           'repo "repo"
           'num "num"
           'sha nil))

(defconst sample-pr-obj
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(defconst random-test-db
  (format "/tmp/code-review-test-db-%s.sqlite" (uuidgen-4)))

(describe "pullreq"
  :var (code-review-database-file
        code-review--db-connection)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil))

  (it "we should be able to create a pullreq db obj from a pr-alist"
    (code-review-db--pullreq-create sample-pr-obj)
    (let ((pr (code-review-db-get-pullreq)))
      (expect (oref pr id) :to-be-truthy)
      (expect (oref pr repo) :to-equal "repo")
      (expect (oref pr owner) :to-equal "owner")
      (expect (oref pr number) :to-equal "num")))

  (it "from the pullreq db obj we can get back the original pr-alist"
    (expect (code-review-db-get-pr-alist)
            :to-have-same-items-as sample-pr-alist))

  (it "update the sha value of a pullreq"
    (code-review-db--pullreq-sha-update "SHA")
    (expect (code-review-db-get-pr-alist)
            :to-have-same-items-as (a-assoc sample-pr-alist 'sha "SHA")))

  (it "update the value of current path should create a buffer with paths"
    (code-review-db--curr-path-update "github.el")
    (let* ((paths (code-review-db-get-buffer-paths))
           (path (-first-item paths)))
      (expect (oref path name)
              :to-equal "github.el")
      (expect (oref path head-pos)
              :to-be nil)
      (expect (oref path at-pos-p)
              :to-be t)))

  (it "update the value of current path should disable `at-pos-p' of previous paths and append new one"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-update "gitlab.el")
    (let* ((paths (code-review-db-get-buffer-paths)))
      (-map
       (lambda (path)
         (cond
          ((string-equal (oref path name) "github.el")
           (expect (oref path at-pos-p) :to-be nil))

          ((string-equal (oref path name) "gitlab.el")
           (expect (oref path at-pos-p) :to-be t))

          (t
           (throw "Test error" ""))))
       paths))))

(describe "path"
  :var (code-review-database-file
        code-review--db-connection)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil)
    (code-review-db--pullreq-create sample-pr-obj))

  (it "we should be able to retrieve the path name of the active current paths (at-pos-p t)"
    (code-review-db--curr-path-update "github.el")
    (expect (code-review-db--curr-path-name)
            :to-equal "github.el"))

  (it "even after path update, retrieve the path name of the active current paths (at-pos-p t)"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-update "bitbucket.el")
    (expect (code-review-db--curr-path-name)
            :to-equal "bitbucket.el"))

  (it "update path head-pos value"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-head-pos-update "github.el" 42)
    (let* ((paths (code-review-db-get-buffer-paths))
           (path (-first-item paths)))
      (dolist (p paths)
        (when (string-equal (oref p name) "github.el")
          (expect (oref p head-pos) :to-equal 42))))))

(describe "comments"
  :var (code-review-database-file
        code-review--db-connection)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil)
    (code-review-db--pullreq-create sample-pr-obj))

  (it "update which comment identifiers was written to buffer"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-comment-written-update "github.el:30")
    (code-review-db--curr-path-comment-count-update 42)
    (let ((comment (code-review-db--curr-comment)))
      (expect (oref comment identifiers)
              :to-equal (list "github.el:30"))))

  (it "add comment written count to current path"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-comment-written-update "github.el:30")
    (code-review-db--curr-path-comment-count-update 42)
    (let ((comment (code-review-db--curr-comment)))
      (expect (oref comment loc-written)
              :to-equal 42)))

  (it "update comment count to current path"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-comment-written-update "github.el:30")
    (code-review-db--curr-path-comment-count-update 42)
    (code-review-db--curr-path-comment-count-update 42)
    (let ((comment (code-review-db--curr-comment)))
      (expect (oref comment loc-written)
              :to-equal 84)))

  (it "update which comment identifiers was written to buffer many calls"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-comment-written-update "github.el:30")
    (code-review-db--curr-path-comment-written-update "github.el:60")
    (code-review-db--curr-path-comment-count-update 42)
    (let ((comment (code-review-db--curr-comment)))
      (expect (oref comment identifiers)
              :to-have-same-items-as (list "github.el:30"
                                           "github.el:60"))))

  (it "verify if a IDENTIFIER was already written to the buffer"
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-comment-written-update "github.el:30")
    (code-review-db--curr-path-comment-written-update "github.el:60")
    (code-review-db--curr-path-comment-count-update 42)
    (expect (code-review-db--comment-already-written? "github.el:30")
            :to-be t)
    (expect (code-review-db--comment-already-written? "github.el:60")
            :to-be t)
    (expect (code-review-db--comment-already-written? "github.el:20")
            :to-be nil)
    (expect (code-review-db--comment-already-written? "gitlab:30")
            :to-be nil))

  (it "get the line number for the comment written in the curr-path."
    (code-review-db--curr-path-update "github.el")
    (code-review-db--curr-path-comment-written-update "github.el:30")
    (code-review-db--curr-path-comment-count-update 42)
    (expect (code-review-db-get-comment-written-pos)
            :to-equal 42)))


(provide 'code-review-db-test)
;;; code-review-db-test.el ends here
