;;; code-review-section-test.el --- Test our section functions
;;; Commentary:
;;; Code:

(require 'uuidgen)
(require 'buttercup)
(require 'code-review-db)
(require 'code-review-gitlab)
(require 'code-review-github)
(require 'code-review-section)

(defconst sample-pr-obj
  (code-review-github-repo
   :owner "owner"
   :repo "repo"
   :number "num"))

(defconst random-test-db
  (format "/tmp/code-review-test-db-%s.sqlite" (uuidgen-4)))

(defun with-written-section (fun expected &optional buffer-nil?)
  "Execute magit insert FUN and match against EXPECTED.
Verify if the buffer has anything written using BUFFER-NIL?."
  (with-temp-buffer
    (funcall fun)
    (setq count 0)
    (goto-char (point-min))
    (magit-wash-sequence
     (lambda ()
       (when-let (section (magit-current-section))
         (with-slots (type value) section
           (let ((rule (nth count expected)))
             (expect (a-get rule 'type) :to-equal type)
             (expect (a-get rule 'value) :to-equal value))
           (setq count (1+ count))))
       (magit-section-forward-sibling)))
    (if buffer-nil?
        (expect (buffer-string) :to-match "")
      (expect (buffer-string) :to-match (rx (any word))))))

(describe "HEADER"
  :var (code-review-database-file
        code-review--db-connection
        code-review-section-indent-width
        code-review-section-image-scaling
        code-review-fill-column)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil
          code-review-section-indent-width 2
          code-review-section-image-scaling 0.8
          code-review-fill-column 70)
    (code-review-db--pullreq-create sample-pr-obj))

  (describe "TITLE"
    (it "available in raw-infos should be added."
      (code-review-db--pullreq-raw-infos-update `((title . "My title")))
      (with-written-section
       (lambda () (code-review-section-insert-title))
       `(((type . code-review-title-section)
          (value . "My title")))))
    (it "missing, should not break and not added to the buffer entirely."
      (code-review-db--pullreq-raw-infos-update nil)
      (with-written-section
       (lambda () (code-review-section-insert-title))
       nil t)))

  (describe "STATE"
    (it "available raw-infos and should be added to the buffer."
      (code-review-db--pullreq-raw-infos-update `((state . "OPEN")))
      (with-written-section
       (lambda () (code-review-section-insert-state))
       `(((type . code-review-state-section)
          (value . "OPEN"))))))

  (describe "MILESTONE"
    (it "available raw-infos and should be added to the buffer."
      (let ((obj (code-review-milestone-section :title "Milestone Title" :perc 50)))
        (code-review-db--pullreq-raw-infos-update `((milestone (title . "Milestone Title")
                                                               (progressPercentage . 50))))
        (with-written-section (lambda () (code-review-section-insert-milestone))
                              `(((type . code-review-milestone-section)
                                 (value . ,obj))))
        (expect (code-review-pretty-milestone obj) :to-equal "Milestone Title (50.00%)")))

    (it "if title is missing, add default msg"
      (let ((obj (code-review-milestone-section :title nil :perc "50")))
        (code-review-db--pullreq-raw-infos-update `((milestone (title . nil) (progressPercentage . "50"))))
        (with-written-section (lambda () (code-review-section-insert-milestone))
                              `(((type . code-review-milestone-section)
                                 (value . ,obj))))
        (expect (code-review-pretty-milestone obj) :to-equal "No milestone")))

    (it "if progress is missing, leave it out."
      (let ((obj (code-review-milestone-section :title "My title" :perc nil)))
        (code-review-db--pullreq-raw-infos-update `((milestone (title . "My title"))))
        (with-written-section (lambda () (code-review-section-insert-milestone))
                              `(((type . code-review-milestone-section)
                                 (value . ,obj))))
        (expect (code-review-pretty-milestone obj) :to-equal "My title"))))

  (describe "COMMENTS"
    (it "inserting general comments in the buffer."
      (let ((obj (code-review-comment-section
                  :author "Code Review"
                  :msg "Comment 1"
                  :id 1234
                  :reactions nil)))
        (code-review-db--pullreq-raw-infos-update `((comments (nodes ((author (login . "Code Review"))
                                                                      (bodyHTML . "<p>Comment 1</p>")
                                                                      (databaseId . 1234)
                                                                      (createdAt . "2021-11-08T00:24:09Z"))))))
        (with-written-section
         (lambda () (code-review-section-insert-top-level-comments))
         `(((type . code-review-comment-header-section))
           ((type . code-review-comment-section)
            (value . ,obj))))))))

(provide 'code-review-section-test)
;;; code-review-section-test.el ends here
