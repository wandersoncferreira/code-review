;;; code-review-comment-test.el --- Test our utility functions
;;; Commentary:
;;; Code:

(require 'a)
(require 'buttercup)
(require 'forge-pullreq)
(require 'code-review-comment)

(defconst sample-raw-comments
  `(;; comment 1
    ((author (login . "wandersoncferreira"))
     (bodyText . "This PR looks great")
     (state . "COMMENTED")
     (comments
      (nodes ((bodyText . "Why keep everything in Emacs?")
              (originalPosition . 3)
              (diffHunk . "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)")
              (position . 3)
              (outdated)
              (path . "README.md")
              (databaseId . 735203147)))))

    ;; comment 2
    ((author (login . "another_user"))
     (bodyText . "This can be improved a lot!")
     (state . "REQUEST_CHANGES")
     (comments
      (nodes ((bodyText . "")
              (originalPosition . 3)
              (diffHunk . "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)")
              (position . 3)
              (outdated)
              (path . "README.md")
              (databaseId . 735203148)))))))

(defconst sample-grouped-comments
  `(("README.md:3"
     ((state . "COMMENTED")
      (author . "wandersoncferreira")
      (bodyText . "Why keep everything in Emacs?")
      (originalPosition . 3)
      (diffHunk . "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)")
      (position . 3)
      (outdated)
      (path . "README.md")
      (databaseId . 735203147))

     ;; comment 2
     ((state . "REQUEST_CHANGES")
      (author . "another_user")
      (bodyText . "")
      (originalPosition . 3)
      (diffHunk . "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)")
      (position . 3)
      (outdated)
      (path . "README.md")
      (databaseId . 735203148)))))

(describe "GROUPING"
  (it "should use PATH + `position' or `originalPosition' fields as key."
    (let ((group (code-review-comment-make-group sample-raw-comments)))
      (expect (a-keys group)
              :to-equal `("README.md:3"))))

  (it "should keep all the comments under the key value"
    (let ((group (code-review-comment-make-group sample-raw-comments)))
      (expect (length (alist-get "README.md:3" group nil nil 'equal))
              :to-equal 2)))

  (it "should flat the structure, add state and login to the comment level."
    (let ((group (code-review-comment-make-group sample-raw-comments)))
      (expect group :to-equal sample-grouped-comments))))

(provide 'code-review-comment-test)
;;; code-review-comment-test.el ends here
