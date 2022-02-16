# Github

## Auth

### Remote

If you have trouble with the authinfo process below there is this nice
[Tweet](https://twitter.com/iLemming/status/1463599279457673220) from @agzam
explaining a bit the setup!

`code-review` needs a GitHub token to act on your behalf for fetching PRs and
submitting reviews.

1. [Create a personal access token using GitHub](https://github.com/settings/tokens)
2. Set the `repo` scope as the subscope of repo
3. If using GitHub enterprise / for business you also need the `write:discussion` `read:discussion` scope.

### Local

Add a line to your auth source files, usually `~/.authinfo.gpg`, with your login
and token:

```
machine api.github.com login YOURLOGIN^code-review password YOURTOKENGOESHERE
```

If using a GitHub Enterprise instance, the URL for the `machine` needs to match the value of `code-review-github-host`. See below.

```
machine ghe.yourdomain.xyz/api login YOURLOGIN^code-review password YOURTOKENGOESHERE
```

## Configuration

### GitHub Enterprise

- Change the value of `code-review-github-host` to match the host of your private instance

```
(setq code-review-github-host "ghe.yourdomain.xyz/api")
```

- Change the value of `code-review-github-graphql-host`

```
(setq code-review-github-graphql-host "ghe.yourdomain.xyz/api")
```

- Optionally, set also the value of `code-review-github-base-url` to be able to start reviews with `code-review-start`, bypassing the Magit status buffer. If you ever only intend to start reviews from that buffer, you can skip this configuration.

```
(setq code-review-github-base-url "ghe.yourdomain.xyz")
```
