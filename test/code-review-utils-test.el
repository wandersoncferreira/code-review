;;; code-review-utils-test.el --- Test our utility functions
;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'code-review-utils)

(defconst sample-grouped-comments
  (a-alist
   "github.el:42" (list '((position . 42) (path . "github.el") (author . "test-1"))
                        '((position . 42) (path . "github.el") (author . "test-2")))))

(describe "COMMENTS"

  (it "are grouped by path and position"
    (expect (code-review-utils-path-pos-key "github.el" 42)
            :to-equal "github.el:42"))

  (it "have helper access function"
    (let ((comments (code-review-utils-get-comments sample-grouped-comments "github.el:42")))
      (expect (length comments) :to-equal 2)
      (expect comments :to-contain '((position . 42)
                                     (path . "github.el")
                                     (author . "test-2"))))))

(describe "GIT")

(describe "URL PARSE"

  (it "given a valid URL of a PR returns an PR-ALIST"
    (expect (code-review-utils-pr-from-url "https://github.com/eval-all-software/tempo/pull/98")
            :to-equal (a-alist
                       'num "98"
                       'repo "tempo"
                       'owner "eval-all-software"))))

(describe "COLORS")

(provide 'code-review-utils-test)
;;; code-review-utils-test.el ends here
