[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/code-review-badge.svg)](https://melpa.org/#/code-review)
![Tests](https://github.com/wandersoncferreira/code-review/actions/workflows/ci.yml/badge.svg)

# Code Review

Package to help you perform code reviews from your VC provider. Currently
supports Github and basic Gitlab workflow.

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
- Set assignee. Use transient `sy` option to `assign yourself` to the PR.
- Set milestone. See details [push access required](./docs/milestone.md)
- Edit PR title
- Edit PR description body
- Merge your PR. _(beta feature) See details [merge](./docs/merge.md)_
- Reactions. See details [react to comments](./docs/reactions.md)
- Promote comments to new issues.
- Save/Resume in-progress Reviews
- Visit binary files in Dired or Remote. Example [here](https://github.com/wandersoncferreira/code-review/pull/90)

Highly recommend using the transient menu in the `*Code Review*` buffer by pressing `r`.

The basic workflow:

- `RET` on a hunk diff line to add a comment
- `RET` on a local comment to edit
- `RET` on a previous sent comment to include a reply
- `C-c C-k` on a local comment to remove it
- `r s f` to enable transient and Set a feedback
- `r a` to approve the PR | `r r` to reject the PR | `r c` to add comments in the PR

You can include your own bindings to functions like
`code-review-comment-set-feedback`, `code-review-approve`,
`code-review-request-changes`, and `code-review-comments` to not rely on the
transient panel. But I think you should see it :]

Take a look at which features are available to each integrated forge [here](./docs/forge_support.md).

Missing something? Please, [let us know](https://github.com/wandersoncferreira/code-review/issues/new).

# Installation

I highly recommend installing `code-review` through `package.el`.

It's available on `MELPA`.

`M-x package-install code-review`

Then you can either `M-x code-review-start` and provide a PR URL or `M-x
code-review-forge-pr-at-point` if you are in a forge buffer over a PR.

# Configuration

### Code Review

Set `code-review-fill-column` to define line wrap comment sections.

If you want to use `code-review` in a full buffer you can change the function
used to display the buffer at `code-review-new-buffer-window-strategy` e.g. you
can set `(setq code-review-new-buffer-window-strategy #'switch-to-buffer)` to
not open a new window.

Set `code-review-download-dir` to change the place Code Review will download
binary files in your pull request when you decide to visit them.

### GitHub

`code-review` needs a GitHub token to act on your behalf for fetching PRs and
submitting reviews.

1. [Create a personal access token using GitHub](https://github.com/settings/tokens)
2. Set the `repo` scope as the subscope of repo
3. If using GitHub enterprise / for business you also need the `write:discussion` `read:discussion` scope.

#### Auth
Add a line to your auth source files with your login and token:

```
machine api.github.com login yourlogin^code-review password MYTOKENGOESHERE
```

### Gitlab

1. [Create a personal access token using Gitlab](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html)
2. Choose the `api` scope

#### Auth

Add a line to your auth source file, usually `~/.authinfo.gpg`, with your login
and token:

``` emacs-lisp
machine gitlab.com/api login yourlogin^code-review password MYTOKENGOESHERE
```

# Keybindings

You can access the transient panel by hitting `r` from any place of the `Code
Review` buffer.

![Transient keybindings](./docs/code_review_transient.png)

| Binding | Object                                | Action                      |
|:-------:|:-------------------------------------:|:---------------------------:|
| RET     | hunk                                  | Add Comment                 |
| RET     | comment                               | Add Reply                   |
| RET     | local comment (not sent to forge yet) | Edit local comment          |
| C-c C-k | local comment                         | Delete local comment        |
| C-c C-c | Comment Buffer                        | Register your local comment |
| C-c C-k | Comment Buffer                        | Cancel your local comment   |
| C-c C-r | comment                               | Add Reaction                |
| C-c C-n | comment                               | Promote to new issue        |
| C-c C-r | pr description                        | Add Reaction                |
| RET     | reaction (on emoji symbol)            | Endorse or Remove Reaction  |
| RET     | Request Reviewer                      | Request reviewer at point   |


## Binding suggestions

You can place `code-review-forge-pr-at-point` to a key binding for your convenience:

``` emacs-lisp
(define-key forge-topic-mode-map (kbd "C-c r") 'code-review-forge-pr-at-point)
```

# Extension to other forges

The package allows you to write integration with other forges to leverage these
functionalities. Take a look at `code-review-interfaces.el` to see which functions
need to be implemented.


# Thanks

Thanks [Laurent Charignon](https://github.com/charignon) for the awesome
[github-review](https://github.com/charignon/github-review) package and
stewardship. Github Review made me more familiar with the problem domain and
`code-review` is an attempt to build on top of it.

Thanks [Ag Ibragimov](https://github.com/agzam) for the amazing idea to use
`magit-section` to build a more suitable interface to this problem.
