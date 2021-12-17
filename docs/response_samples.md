# Response Samples

## Diff request

The diff request has the following expected structure:

```elisp
((message . "diff --git a/File...")
  (documentation_url . "https://github..."))
```

A raw log sample for [this](https://github.com/wandersoncferreira/dotfiles/pull/5) public PR.
```

Fri Dec 17 09:46:42 2021 - code-review--build-buffer: [DIFF] - ((message . "diff --git a/README.md b/README.md
index fb7586b..1269943 100644
--- a/README.md
+++ b/README.md
@@ -1,7 +1,8 @@
 All I can save about my current computer setup:
 
-- [nixos](https://nixos.org/)
 - [fedora](https://getfedora.org/)
-- [archlinux](https://archlinux.org)
 - [macos](https://www.apple.com/macbook-pro-13/)
 - [emacs](https://www.gnu.org/software/emacs/)
+- [firefox]()
+
+Do we need a browser? o.O") (documentation_url . "https://github.com/magit/ghub/wiki/Github-Errors"))

```

## Infos request

Several additional informations about your PR is gathered by a GraphQL query.

A sample logged under the tag `code-review--build-buffer: [INFOS_main]`:

```elisp
((data 
   (repository (pullRequest 
                 (id . "PR_kwDOFJ0H284tmPHj") 
                 (headRefOid . "d72073ccb115fbddc200ce2919242dcbfcc0a709") 
                 (baseRefName . "main") 
                 (headRefName . "wandersoncferreira-patch-2") 
                 (isDraft) 
                 (databaseId . 764998115) 
                 (number . 5) 
                 (createdAt . "2021-10-25T01:21:05Z") 
                 (updatedAt . "2021-11-19T13:35:53Z") 
                 (title . "Update README.md") 
                 (state . "CLOSED") 
                 (bodyHTML . "<p dir=\"auto\">This is a demo PR. Let's see how <code>code-review</code> emacs package handles it.</p>") 
                 (bodyText . "This is a demo PR. Let's see how code-review emacs package handles it.")
                 (latestOpinionatedReviews (nodes)) 
                 (reviewRequests (nodes)) 
                 (files (nodes ((path . "README.md") (additions . 3) (deletions . 2))))
                 (milestone) 
                 (labels (nodes ((name . "bug") (color . "d73a4a")) 
                                ((name . "enhancement") (color . "a2eeef")) 
                                ((name . "help wanted") (color . "008672")))) 
                 (assignees (nodes)) 
                 (projectCards (nodes ((project (name . "Demonstration Project Name"))))) 
                 (suggestedReviewers) 
                 (commits
                   (totalCount . 1) 
                   (nodes ((commit (abbreviatedOid . "d72073c") (message . "Update README.md") (statusCheckRollup))))) 
                 (reactions (nodes)) 
                 (comments (nodes)) 
                 (reviews 
                   (nodes ((typename . "PullRequestReview") 
                           (author (login . "wandersoncferreira")) 
                           (bodyHTML . "") 
                           (state . "COMMENTED") 
                           (createdAt . "2021-10-25T01:21:27Z") 
                           (databaseId . 787585454) 
                           (updatedAt . "2021-10-25T01:21:27Z")
                           (position . 4) 
                           (outdated) 
                           (path . "README.md") 
                           (databaseId . 735203748) 
                           (reactions (nodes))
                           (comments (nodes ((createdAt . "2021-10-25T01:21:27Z") 
                                             (updatedAt . "2021-10-25T01:22:20Z")
                                             (bodyHTML . "<p dir=\"auto\">Why not remove <code>fedora</code> too?</p>")
                                             (originalPosition . 4)
                                             (diffHunk . "@@ -1,7 +1,8 @@
 All I can save about my current computer setup:
 
-- [nixos](https://nixos.org/)
 - [fedora](https://getfedora.org/)")))))))))))
```
