# Github

## Auth

If you have trouble with the authinfo process below there is this nice
[Tweet](https://twitter.com/iLemming/status/1463599279457673220) from @agzam
explaining a bit the setup!

`code-review` needs a GitHub token to act on your behalf for fetching PRs and
submitting reviews.

1. [Create a personal access token using GitHub](https://github.com/settings/tokens)
2. Set the `repo` scope as the subscope of repo
3. If using GitHub enterprise / for business you also need the `write:discussion` `read:discussion` scope.

For enterprise users do not forget to change the value of
`code-review-github-host` to match the host of your private instance. The
current recommended way to use the package with enterprise solution is through
`code-review-forge-pr-at-point` we have a bug identifying enterprise URLs in `code-review-start` yet.

Add a line to your auth source files, usually `~/.authinfo.gpg`, with your login
and token:

```
machine api.github.com login yourlogin^code-review password MYTOKENGOESHERE
```
