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

(defun with-written-section (fun expected)
  "Execute magit insert FUN and match against EXPECTED."
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
      (expect (buffer-string) :to-match (rx (any word))))))

(describe "HEADER"
  :var (code-review-database-file
        code-review--db-connection)
  (before-all
    (setf code-review-database-file random-test-db
          code-review--db-connection nil)
    (code-review-db--pullreq-create sample-pr-obj))

  (it "The titles comes from raw-infos and should be added to the buffer."
    (code-review-db--pullreq-raw-infos-update `((title . "My title")))
    (with-written-section
     (lambda () (code-review-section-insert-title))
     `(((type . code-review-title)
        (value . "My title"))))))

(provide 'code-review-section-test)
;;; code-review-section-test.el ends here
