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
     (bodyHTML . "<p>This PR looks great</p>")
     (state . "COMMENTED")
     (createdAt . "2021-11-08T00:24:09Z")
     (updatedAt . "2021-11-08T00:24:09Z")
     (comments
      (nodes ((createdAt . "2021-11-08T00:24:09Z")
              (updatedAt . "2021-11-08T00:24:09Z")
              (bodyHTML . "<p>Why keep everything in Emacs?</p>")
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
     (bodyHTML . "<p>This can be improved a lot!</p>")
     (state . "REQUEST_CHANGES")
     (createdAt . "2021-11-08T00:24:09Z")
     (updatedAt . "2021-11-08T00:24:09Z")
     (comments
      (nodes ((createdAt . "2021-11-08T00:24:09Z")
              (updatedAt . "2021-11-08T00:24:09Z")
              (bodyHTML . "")
              (originalPosition . 3)
              (diffHunk . "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)")
              (position . 3)
              (outdated)
              (path . "README.md")
              (databaseId . 735203148)))))))

(defconst sample-grouped-raw-comments
  (a-alist "README.md:3"
           (list
            (code-review-code-comment-section
             :createdAt "2021-11-08T00:24:09Z"
             :updatedAt "2021-11-08T00:24:09Z"
             :state "COMMENTED"
             :author "wandersoncferreira"
             :msg "<p>Why keep everything in Emacs?</p>"
             :position 3
             :reactions nil
             :path "README.md"
             :diffHunk "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)"
             :internalId nil
             :id 735203147)

            ;; comment 2
            (code-review-code-comment-section
             :createdAt "2021-11-08T00:24:09Z"
             :updatedAt "2021-11-08T00:24:09Z"
             :state "REQUEST_CHANGES"
             :author "another_user"
             :msg ""
             :position 3
             :reactions nil
             :path "README.md"
             :diffHunk "@@ -5,3 +5,5 @@ All I can save about my current computer setup:
 - [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)"
             :internalId nil
             :id 735203148))))

(describe "GROUPING"
  (it "should use PATH + `position' or `originalPosition' fields as key."
    (let ((group (code-review-utils-make-group sample-raw-comments)))
      (expect (a-keys group)
              :to-equal `("README.md:3"))))

  (it "should keep all the comments under the key value"
    (let ((group (code-review-utils-make-group sample-raw-comments)))
      (expect (length (alist-get "README.md:3" group nil nil 'equal))
              :to-equal 2)))

  (it "should flat the structure, add state and login to the comment level."
    (let ((group (code-review-utils-make-group sample-raw-comments)))
      (expect group :to-have-same-items-as sample-grouped-raw-comments))))

(provide 'code-review-comment-test)
;;; code-review-comment-test.el ends here
