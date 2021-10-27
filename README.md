[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
![Tests](https://github.com/wandersoncferreira/code-review/actions/workflows/ci.yml/badge.svg)

# Code Review

Package to help you perform code reviews from your VC provider. Currently
supports only Github.

![Demo of code review package](./docs/code_review_demo.png)

Link to same PR on Github: https://github.com/wandersoncferreira/dotfiles/pull/5

# Overview

The Emacs everywhere goal continues. These are the main features of
`code-review` to help you never leave Emacs to do Pull Request reviews.

- Start review from URL via `code-review-start`
- Modern UI using [magit-section](https://emacsair.me/2020/01/23/magit-section/) and [transient](https://github.com/magit/transient)
- Read Pull Request comments
- Reply to comments
- Include code suggestions
- View `outdated` comments with the right diff hunk context
- Approve, Reject or Request Changes for your PRs
- Integrated with `forge-topic-view` via `code-review-forge-pr-at-point`
- Fast track commands like "LGTM! Approved"
- Review using single commits to focus on diff
- Set labels on RET. See details [Multi value selection](./docs/multi-value-selection.md)
- Set assignee. Use transient `ay` option to `assign yourself` to the PR.
- Set milestone. See details [push access required](./docs/milestone.md)
- Edit PR title
- Edit PR description body


Missing something? Please, [let us know](https://github.com/wandersoncferreira/code-review/issues/new).

# Installation

### With melpa

TBD

### Directly from source

Clone and add the package to your `load-path`:

``` emacs-lisp
(add-to-list 'load-path "~/<path-to-the-place-you-cloned>/code-review")
(require 'code-review)

```

if you are using `use-package`:

``` emacs-lisp

(use-package code-review
  :load-path "/path-to-the-place-you-cloned")
```

Then you can either `M-x code-review-start` and provide a PR URL or `M-x
code-review-forge-pr-at-point` if you are in a forge buffer over a PR.

# Configuration

### GitHub

`code-review` needs a GitHub token to act on your behalf for fetching PRs and
submitting reviews.

1. [Create a personal access token using GitHub](https://github.com/settings/tokens)
2. Set the `repo` scope as the subscope of repo
3. If using GitHub enterprise / for business you also need the `write:discussion` `read:discussion` scope.

### Auth
Add a line to your auth source files with your login and token:

```
machine api.github.com login yourlogin^code-review password MYTOKENGOESHERE
```

# Transient keys

You can access the transient panel by hitting `r` from any place of the `Code
Review` buffer.

![Transient keybindings](./docs/code_review_transient.png)


# Extension to other forges

The package allows you to write integration with other forges to leverage these
functionalities. Take a look at `code-review-core.el` to see which functions
need to be implemented.


# Thanks

Thanks [Laurent Charignon](https://github.com/charignon) for the awesome
[github-review](https://github.com/charignon/github-review) package and
stewardship. Github Review made me more familiar with the problem domain and
`code-review` is an attempt to build on top of it.

Thanks [Ag Ibragimov](https://github.com/agzam) for the amazing idea to use
`magit-section` to build a more suitable interface to this problem.
