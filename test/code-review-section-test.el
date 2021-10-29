;;; code-review-section-test.el --- Test our section functions
;;; Commentary:
;;; Code:

(require 'uuidgen)
(require 'buttercup)
(require 'code-review-db)
(require 'code-review-section)
(require 'code-review-github)

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
    (save-window-excursion
      (set-window-buffer nil (current-buffer))
      (goto-char (point-min))
      (while (and (not (eobp)) (magit-current-section))
        (with-slots (type value) (magit-current-section)
          (let ((rule (nth count expected)))
            (expect (a-get rule 'type) :to-equal type)
            (expect (a-get rule 'value) :to-equal value))
          (setq count (1+ count)))
        (forward-line))
      (if buffer-nil?
          (expect (buffer-string) :to-match "")
        (expect (buffer-string) :to-match (rx (any word)))))))

(describe "HEADER"
  :var (code-review-database-file
        code-review--db-connection)
  (before-each
    (setf code-review-database-file random-test-db
          code-review--db-connection nil)
    (code-review-db--pullreq-create sample-pr-obj))

  (describe "TITLE"
    (it "available in raw-infos should be added."
      (code-review-db--pullreq-raw-infos-update `((title . "My title")))
      (with-written-section
       (lambda () (code-review-section-insert-title))
       `(((type . code-review-title)
          (value . "My title")))))
    (it "missing, should not break and not added to the buffer entirely."
      (with-written-section
       (lambda () (code-review-section-insert-title))
       `(((type . code-review-title)
          (value . "My title")))
       t)))

  (describe "STATE"
    (it "available raw-infos and should be added to the buffer."
      (code-review-db--pullreq-raw-infos-update `((state . "OPEN")))
      (with-written-section
       (lambda () (code-review-section-insert-state))
       `(((type . code-review-state)
          (value . "OPEN"))))))

  (describe "MILESTONE"
    (it "available raw-infos and should be added to the buffer."
      (code-review-db--pullreq-raw-infos-update `((milestone (title . "Milestone Title")
                                                             (progressPercentage . "50"))))
      (with-written-section
       (lambda () (code-review-section-insert-milestone))
       `(((type . code-review-milestone)
          (value . ((title . "Milestone Title")
                    (progress . "50")
                    (visible-text . "Milestone Title (50%)")))))))

    (it "if title is missing, add default msg"
      (code-review-db--pullreq-raw-infos-update `((milestone (title . nil)
                                                             (progressPercentage . "50"))))
      (with-written-section
       (lambda () (code-review-section-insert-milestone))
       `(((type . code-review-milestone)
          (value . ((title)
                    (progress . "50")
                    (visible-text . "No milestone")))))))

    (it "if progress is missing, leave it out."
      (code-review-db--pullreq-raw-infos-update `((milestone (title . "My title"))))
      (with-written-section
       (lambda () (code-review-section-insert-milestone))
       `(((type . code-review-milestone)
          (value . ((title . "My title")
                    (progress)
                    (visible-text . "My title")))))))))

(provide 'code-review-section-test)
;;; code-review-section-test.el ends here
