# Multi Value Selection

Code Review is currently using `completing-read-multiple` function which allows
us to choose more than 1 option for the header fields of your Pull Request e.g.
Labels, Milestones, Projects, and others.

If you provide an empty candidate, all the current labels will be removed from the PR.

`completing-read-multiple` works by separating each value with a comma. You can
also see a list of candidates by pressing TAB.


## Request Reviewers

The `code-review-request-reviewers` command aldo relies on `completing-read-multiple`.
