# Gitlab

## Auth

If you have trouble with the authinfo process below there is this nice
[Tweet](https://twitter.com/iLemming/status/1463599279457673220) from @agzam
explaining a bit the setup!

1. [Create a personal access token using Gitlab](https://docs.gitlab.com/ee/user/profile/personal_access_tokens.html)
2. Choose the `api` scope

For enterprise users do not forget to change the value of
`code-review-gitlab-host` and `code-review-gitlab-graphql-host` to match the
ones of your private instance. The current recommended way to use the package
with enterprise solution is through `code-review-forge-pr-at-point` we have a
bug identifying enterprise URLs in `code-review-start` yet.

Add a line to your auth source file, usually `~/.authinfo.gpg`, with your login
and token:

``` emacs-lisp
machine gitlab.com/api login yourlogin^code-review password MYTOKENGOESHERE
```


## Approve Merge Request

Gitlab's repository can require a password from users before approving a Merge
Request (see details
[here](https://docs.gitlab.com/ee/user/project/merge_requests/approvals/settings.html#require-user-password-to-approve))
and if this is enabled you will be prompted to type your username and password
again.


