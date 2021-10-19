;;; code-review-db-test.el --- Test our utility functions
;;; Commentary:
;;; Code:

(require 'a)
(require 'dash)
(require 'uuidgen)
(require 'buttercup)
(require 'code-review-db)

(defconst sample-pr-alist
  (a-alist 'owner "owner"
           'repo "repo"
           'num "num"
           'sha nil))

(defconst random-test-db
  (format "/tmp/code-review-test-db-%s.sqlite" (uuidgen-4)))

(describe "pullreq"
  :var (code-review-database-file
        code-review--db-connection)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil))

  (it "we should be able to create a pullreq db obj from a pr-alist"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (expect (oref pr id) :to-be-truthy)
      (expect (oref pr repo) :to-equal "repo")
      (expect (oref pr owner) :to-equal "owner")
      (expect (oref pr number) :to-equal "num")))

  (it "from the pullreq db obj we can get back the original pr-alist"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (expect (code-review-db-get-pr-alist (oref pr id))
              :to-have-same-items-as sample-pr-alist)))

  (it "update the sha value of a pullreq"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--pullreq-sha-update (oref pr id) "SHA")
      (expect (code-review-db-get-pr-alist (oref pr id))
              :to-have-same-items-as (a-assoc sample-pr-alist 'sha "SHA"))))

  (it "update the value of current path should create a buffer with paths"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (let* ((paths (code-review-db-get-buffer-paths (oref pr id)))
             (path (-first-item paths)))
        (expect (oref path name)
                :to-equal "github.el")
        (expect (oref path head-pos)
                :to-be nil)
        (expect (oref path at-pos-p)
                :to-be t))))

  (it "update the value of current path should disable `at-pos-p' of previous paths and append new one"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (code-review-db--curr-path-update (oref pr id) "gitlab.el")
      (let* ((paths (code-review-db-get-buffer-paths (oref pr id))))
        (-map
         (lambda (path)
           (cond
            ((string-equal (oref path name) "github.el")
             (expect (oref path at-pos-p) :to-be nil))

            ((string-equal (oref path name) "gitlab.el")
             (expect (oref path at-pos-p) :to-be t))

            (t
             (throw "Test error" ""))))
         paths)))))

(describe "path"
  :var (code-review-database-file
        code-review--db-connection)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil))

  (it "we should be able to retrieve the path name of the active current paths (at-pos-p t)"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (expect (code-review-db--curr-path-name (oref pr id))
              :to-equal "github.el")))

  (it "even after path update, retrieve the path name of the active current paths (at-pos-p t)"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (code-review-db--curr-path-update (oref pr id) "bitbucket.el")
      (expect (code-review-db--curr-path-name (oref pr id))
              :to-equal "bitbucket.el")))

  (it "update path head-pos value"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (code-review-db--curr-path-head-pos-update (oref pr id) "github.el" 42)
      (let* ((paths (code-review-db-get-buffer-paths (oref pr id)))
             (path (-first-item paths)))
        (expect (oref path head-pos)
                :to-equal 42)))))

(describe "comments"
  :var (code-review-database-file
        code-review--db-connection)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil))

  (it "add comment written count to current path"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (code-review-db--curr-path-comment-count-update (oref pr id) 42)
      (let ((comment (code-review-db-get-curr-path-comment (oref pr id))))
        (expect (oref comment loc-written)
                :to-equal 42))))

  (it "update comment count to current path"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (code-review-db--curr-path-comment-count-update (oref pr id) 42)
      (code-review-db--curr-path-comment-count-update (oref pr id) 42)
      (let ((comment (code-review-db-get-curr-path-comment (oref pr id))))
        (expect (oref comment loc-written)
                :to-equal 84))))

  (it "update which comment identifiers was written to buffer"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (code-review-db--curr-path-comment-count-update (oref pr id) 42)
      (code-review-db--curr-path-comment-written-update (oref pr id) "github.el:30")
      (let ((comment (code-review-db-get-curr-path-comment (oref pr id))))
        (expect (oref comment identifiers)
                :to-equal (list "github.el:30")))))

  (it "update which comment identifiers was written to buffer many calls"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (code-review-db--curr-path-comment-count-update (oref pr id) 42)
      (code-review-db--curr-path-comment-written-update (oref pr id) "github.el:30")
      (code-review-db--curr-path-comment-written-update (oref pr id) "github.el:60")
      (let ((comment (code-review-db-get-curr-path-comment (oref pr id))))
        (expect (oref comment identifiers)
                :to-have-same-items-as (list "github.el:30"
                                             "github.el:60")))))

  (it "verify if a IDENTIFIER was already written to the buffer"
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (code-review-db--curr-path-comment-count-update (oref pr id) 42)
      (code-review-db--curr-path-comment-written-update (oref pr id) "github.el:30")
      (code-review-db--curr-path-comment-written-update (oref pr id) "github.el:60")
      (expect (code-review-db--comment-already-written? (oref pr id) "github.el:30")
              :to-be t)
      (expect (code-review-db--comment-already-written? (oref pr id) "github.el:60")
              :to-be t)
      (expect (code-review-db--comment-already-written? (oref pr id) "github.el:20")
              :to-be nil)
      (expect (code-review-db--comment-already-written? (oref pr id) "gitlab:30")
              :to-be nil)))

  (it "get the line number for the comment written in the curr-path."
    (let ((pr (code-review-db--pullreq-create sample-pr-alist)))
      (code-review-db--curr-path-update (oref pr id) "github.el")
      (code-review-db--curr-path-comment-count-update (oref pr id) 42)
      (expect (code-review-db-get-comment-written-pos (oref pr id))
              :to-equal 42))))


(provide 'code-review-db-test)
;;; code-review-db-test.el ends here
