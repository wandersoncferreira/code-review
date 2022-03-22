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
3. If using *GitHub Enterprise* / for business you also need the `write:discussion` `read:discussion` scope.

### Local

Add a line to your auth source files, usually `~/.authinfo.gpg`, with your login
and token:

```
machine api.github.com login YOURLOGIN^code-review password YOURTOKENGOESHERE
```

See below for specific information for **GitHub Enterprise**.

## GitHub Enterprise Configuration


When you use a *GitHub Enterprise* instance, the URL (with much of the path) needs to be in `.authinfo.gpg` with login credentials (`username^application` and `password`). The URL-and-path appear as the `machine`, and there needs to be one for each path used in `code-review`. So, for a "belt and suspenders" approach that is known to work, use these lines with the personal access token you generated.

```
machine ghe.yourdomain.xyz             login YOURLOGIN^code-review password YOURTOKENGOESHERE
machine ghe.yourdomain.xyz/api         login YOURLOGIN^code-review password YOURTOKENGOESHERE
machine ghe.yourdomain.xyz/api/v3      login YOURLOGIN^code-review password YOURTOKENGOESHERE
machine ghe.yourdomain.xyz/api/graphql login YOURLOGIN^code-review password YOURTOKENGOESHERE
```

After setting up `.authinfo.gpg`, you need to set the three variables that identify the *GitHub Enterprise* host **with various paths**.

- Change the value of `code-review-github-host` to match the host of your private instance *with* any appended path for the REST API such as `/api/v3`

```
        (setq code-review-github-host "ghe.yourdomain.xyz/api/v3")
```

- Change the value of `code-review-github-graphql-host` (`/graphql` is automatically appended in the code)

```
        (setq code-review-github-graphql-host "ghe.yourdomain.xyz/api")
```

- Optionally, set also the value of `code-review-github-base-url` to be able to start reviews with `code-review-start`, bypassing the Magit status buffer. If you ever only intend to start reviews from that buffer, you can skip this configuration.

```
        (setq code-review-github-base-url "ghe.yourdomain.xyz")
```

These three variables can be customized with `M-x customize-group`, entering `code-review-github` at the prompt.

#### git configuration

`code-review` needs to know who you are when accessing your *GitHub Enterprise* host. Again, the belt-and-suspenders approach so you don't have to try every permutation. Issue these commands at the shell prompt.

```shell
git config --global github.ghe.yourdomain.xyz.user             YOURLOGIN
git config --global github.ghe.yourdomain.xyz/api.user         YOURLOGIN
git config --global github.ghe.yourdomain.xyz/api/graphql.user YOURLOGIN
git config --global github.ghe.yourdomain.xyz/api/v3.user      YOURLOGIN
```

#### Work on open source, too?

If you work not only on corporate projects but also on open source on GitHub itself, you can still use `code-review` if your open source projects are under their own root directory, and that root directory contains the following in `.dir-locals.el`:

```emacs-lisp
((nil . ((code-review-github-base-url . "api.github.com")
         (code-review-github-graphql-host . "api.github.com")
         (code-review-github-host . "api.github.com"))))
```

This will reset the `code-review` customizations for all the projects below that directory, so you can contribute to open source with the same tools used for your in-house work.
