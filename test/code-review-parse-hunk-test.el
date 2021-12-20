;;; code-review-parse-hunk-test.el --- Test parse diff functions
;;; Commentary:
;;; Code:
;;;
(require 'a)
(require 'buttercup)
(require 'code-review-parse-hunk)

(defvar hunk-sample
  "@@ -2,14 +2,7 @@

 var hello = require('./hello.js');

-var names = [
-  'harry',
-  'barry',
-  'garry',
-  'harry',
-  'barry',
-  'marry',
-];
+var names = ['harry', 'barry', 'garry', 'harry', 'barry', 'marry'];

 var names2 = [
   'harry',
@@ -23,9 +16,7 @@ var names2 = [
 // after this line new chunk will be created
 var names3 = [
   'harry',
-  'barry',
-  'garry',
   'harry',
   'barry',
-  'marry',
+  'marry', 'garry',
 ];")

(defvar expected-hunk-table
  `(((type . "normal") (normal . t) (ln1 . 2) (ln2 . 2) (relative . 1))
    ((type . "normal") (normal . t) (ln1 . 3) (ln2 . 3) (relative . 2))
    ((type . "normal") (normal . t) (ln1 . 4) (ln2 . 4) (relative . 3))
    ((type . "del") (del . t) (ln . 5) (relative . 4))
    ((type . "del") (del . t) (ln . 6) (relative . 5))
    ((type . "del") (del . t) (ln . 7) (relative . 6))
    ((type . "del") (del . t) (ln . 8) (relative . 7))
    ((type . "del") (del . t) (ln . 9) (relative . 8))
    ((type . "del") (del . t) (ln . 10) (relative . 9))
    ((type . "del") (del . t) (ln . 11) (relative . 10))
    ((type . "del") (del . t) (ln . 12) (relative . 11))
    ((type . "add") (add . t) (ln . 5) (relative . 12))
    ((type . "normal") (normal . t) (ln1 . 13) (ln2 . 6) (relative . 13))
    ((type . "normal") (normal . t) (ln1 . 14) (ln2 . 7) (relative . 14))
    ((type . "normal") (normal . t) (ln1 . 15) (ln2 . 8) (relative . 15))
    ((type . "normal") (normal . t) (ln1 . 23) (ln2 . 16) (relative . 17))
    ((type . "normal") (normal . t) (ln1 . 24) (ln2 . 17) (relative . 18))
    ((type . "normal") (normal . t) (ln1 . 25) (ln2 . 18) (relative . 19))
    ((type . "del") (del . t) (ln . 26) (relative . 20))
    ((type . "del") (del . t) (ln . 27) (relative . 21))
    ((type . "normal") (normal . t) (ln1 . 28) (ln2 . 19) (relative . 22))
    ((type . "normal") (normal . t) (ln1 . 29) (ln2 . 20) (relative . 23))
    ((type . "del") (del . t) (ln . 30) (relative . 24))
    ((type . "add") (add . t) (ln . 21) (relative . 25))
    ((type . "normal") (normal . t) (ln1 . 31) (ln2 . 22) (relative . 26))))

(describe "PARSE HUNK"
  (it "Given a hunk, we return a table describing what happened with each hunk line."
    (let ((table (code-review-parse-hunk-table hunk-sample)))
      (expect table :to-equal expected-hunk-table))))

(describe "RELATIVE POS"
  :var (table)
  (before-all
    (setf table (code-review-parse-hunk-table hunk-sample)))
  (it "Given a hunk parsed table we can ask for the relative position of a loc."
    (let ((line-obj `((old . t)
                      (line-pos . 23))))
      (expect (code-review-parse-hunk-relative-pos table line-obj)
              :to-equal 17)))
  (it "Given a deleted line from the original file"
    (let ((line-obj `((old . t)
                      (line-pos . 6))))
      (expect (code-review-parse-hunk-relative-pos table line-obj)
              :to-equal 5)))
  (it "Given yet another deleted line from the original file"
    (let ((line-obj `((old . t)
                      (line-pos . 30))))
      (expect (code-review-parse-hunk-relative-pos table line-obj)
              :to-equal 24)))
  (it "Given a new line."
    (let ((line-obj `((new . t)
                      (line-pos . 5))))
      (expect (code-review-parse-hunk-relative-pos table line-obj)
              :to-equal 12)))
  (it "Given yet another new line."
    (let ((line-obj `((new . t)
                      (line-pos . 21))))
      (expect (code-review-parse-hunk-relative-pos table line-obj)
              :to-equal 25))))

(describe "DIFF POS"
  :var (table)
  :before-all
  (setf table (code-review-parse-hunk-table hunk-sample))
  (it "Given a relative line position in ADDED line, return the new line number."
    (let ((hunk-obj `((added . t)
                      (line-pos . 25))))
      (expect (code-review-parse-hunk-line-pos table hunk-obj)
              :to-equal 21)))
  (it "Given a relative line position in DELETED line, return the new line number."
    (let ((hunk-obj `((deleted . t)
                      (line-pos . 21))))
      (expect (code-review-parse-hunk-line-pos table hunk-obj)
              :to-equal 27)))
  (it "Given a relative line position in NORMAL line, return the new line number."
    (let ((hunk-obj `((normal . t)
                      (line-pos . 23))))
      (expect (code-review-parse-hunk-line-pos table hunk-obj)
              :to-equal `((old-line . 29)
                          (new-line . 20))))))

(provide 'code-review-parse-diff-test)
;;; code-review-parse-diff-test.el ends here
