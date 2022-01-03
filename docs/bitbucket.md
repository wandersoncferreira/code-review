# Bitbucket

## Auth

If you have trouble with the authinfo process below there is this nice
[Tweet](https://twitter.com/iLemming/status/1463599279457673220) from @agzam
explaining a bit the setup!

1. Create a personal access token using Bitbucket. Go to your Personal Settings -> App Password
2. Choose the `pull_request:write` scope

Add a line to your auth source file, usually `~/.authinfo.gpg`, with your login
and token:

``` emacs-lisp
machine api.bitbucket.org/2.0 login yourlogin^code-review password MYAPPPASSWORD
```
