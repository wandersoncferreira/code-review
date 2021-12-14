;;; code-review-utils-test.el --- Test our utility functions
;;; Commentary:
;;; Code:

(require 'a)
(require 'buttercup)
(require 'code-review-utils)

(defconst sample-grouped-comments
  (a-alist
   "github.el:42" (list '((position . 42) (path . "github.el") (author . "test-1"))
                        '((position . 42) (path . "github.el") (author . "test-2")))))

(defconst sample-comment-written-lines
  (a-alist
   "github.el" 0))

(defconst sample-comment-lines
  (list "This need to be changed"
        "Improve this code please"))

(defconst sample-suggestion-comment
  "Suggested change\n        \n          \n    \n\n        \n      \n    \n    \n      \n          \n            \n               :extra-deps {thheller/shadow-cljs {:mvn/version \"2.15.12\"}\n          \n          \n            \n               :extra-deps {thheller/shadow-cljs {:mvn/version \"2.15.14\"}")

(describe "COMMENTS"

  (it "are grouped by path and position"
    (expect (code-review-utils--comment-key "github.el" 42)
            :to-equal "github.el:42"))

  (it "have helper access function"
    (let ((comments (code-review-utils--comment-get sample-grouped-comments "github.el:42")))
      (expect (length comments) :to-equal 2)
      (expect comments :to-contain '((position . 42)
                                     (path . "github.el")
                                     (author . "test-2")))))

  (it "the input comment buffers have placeholder text that needs to be removed on write"
    (let ((placeholder-msg ";;; This is a placeholder in a buffer")
          (full-msg ";;; This is a placeholder in a buffer\nThis is my real comment"))
      (expect (code-review-utils--comment-clean-msg full-msg placeholder-msg)
              :to-equal "This is my real comment"))

    (let ((placeholder-msg ";;; This is a placeholder in a buffer")
          (full-msg ";;; This is a placeholder in a bufferThis is my real comment"))
      (expect (code-review-utils--comment-clean-msg full-msg placeholder-msg)
              :to-equal "This is my real comment")))

  (it "we need to keep track of how many lines was written for each buffer"
    (expect (code-review-utils--comment-update-written-count sample-comment-written-lines "github.el" (length sample-comment-lines))
            :to-equal (a-alist "github.el" 2))
    (expect (code-review-utils--comment-update-written-count (a-alist "github.el" 5) "github.el" (length sample-comment-lines))
            :to-equal (a-alist "github.el" 7)))

  (it "clean suggestion code blocks and add 'hunk-like' appearance."
    (expect (code-review-utils--clean-suggestion sample-suggestion-comment)
            :to-equal `("Suggested change"
                        "-   :extra-deps {thheller/shadow-cljs {:mvn/version \"2.15.12\"}"
                        "+   :extra-deps {thheller/shadow-cljs {:mvn/version \"2.15.14\"}")))

  (it "compute if any outdated test was not written in the buffer."
    (expect (code-review-utils--missing-outdated-commments?
             "github.el"
             (list "github.el:30" "github.el:20" "gitlab.el:12")
             `(("github.el:30" . (list 1 2 3))
               ("github.el:20" . (list 1 2 3))
               ("github.el:50" . (list 1 2 3))))
            :to-equal (list "github.el:50"))))

(describe "GIT")

(describe "URL PARSE"

  (it "given a valid Github URL of a PR returns an PR-ALIST"
    (expect (code-review-utils-pr-from-url "https://github.com/eval-all-software/tempo/pull/98")
            :to-equal (a-alist
                       'num "98"
                       'repo "tempo"
                       'owner "eval-all-software"
                       'forge 'github
                       'url "https://github.com/eval-all-software/tempo/pull/98")))

  (it "given a valid Gitlab URL of a PR returns an PR-ALIST"
    (expect (code-review-utils-pr-from-url "https://gitlab.com/code-review-experiment/default/-/merge_requests/1")
            :to-equal (a-alist
                       'num "1"
                       'repo "default"
                       'owner "code-review-experiment"
                       'forge 'gitlab
                       'url "https://gitlab.com/code-review-experiment/default/-/merge_requests/1")))
  (it "given a valid Gitlab URL with subgroup of a PR returns an PR-ALIST"
    (expect (code-review-utils-pr-from-url "https://gitlab.com/owner/group/subgroup/project/-/merge_requests/1")
            :to-equal (a-alist
                       'num "1"
                       'repo "group%2Fsubgroup%2Fproject"
                       'owner "owner"
                       'forge 'gitlab
                       'url "https://gitlab.com/owner/group/subgroup/project/-/merge_requests/1")))
  (it "given a valid Gitlab URL with subgroups nested of a PR returns an PR-ALIST"
    (expect (code-review-utils-pr-from-url "https://gitlab.com/owner/group/subgroup1/subgroup2/subgroup3/project/-/merge_requests/1")
            :to-equal (a-alist
                       'num "1"
                       'repo "group%2Fsubgroup1%2Fsubgroup2%2Fsubgroup3%2Fproject"
                       'owner "owner"
                       'forge 'gitlab
                       'url "https://gitlab.com/owner/group/subgroup1/subgroup2/subgroup3/project/-/merge_requests/1"))))

(describe "COLORS")

(provide 'code-review-utils-test)
;;; code-review-utils-test.el ends here
