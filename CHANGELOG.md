# Change log

# Unreleased

- [#68](https://github.com/wandersoncferreira/code-review/pull/68): add files changed heading before diff
- [#74](https://github.com/wandersoncferreira/code-review/pull/74): fix `code-review-forge-pr-at-point` command
- [#75](https://github.com/wandersoncferreira/code-review/pull/75): fix bug related to MELPA not delivering graphql files by default. Special thanks to [@mplanchard](https://github.com/mplanchard)
- [#73](https://github.com/wandersoncferreira/code-review/pull/73): add reviewers section and `set reviewers` command
- [#80](https://github.com/wandersoncferreira/code-review/pull/80): make transient keys follow Forge convention of key spacing
- [#85](https://github.com/wandersoncferreira/code-review/pull/85): remove comment and feedback section with `k`

# v0.0.2

- [#52](https://github.com/wandersoncferreira/code-review/pull/52): fix `wrong-type-argument number-or-marker-p nil` error for comments in the buffer's last line
- [#55](https://github.com/wandersoncferreira/code-review/pull/55): fix `(wrong-type-argument (or eieio-object class) nil obj)`. Special thanks to [@ktfleming](https://github.com/ktfleming)
- [#56](https://github.com/wandersoncferreira/code-review/pull/56): remove `code-review-submit` interactive call from transient options.
- [#57](https://github.com/wandersoncferreira/code-review/pull/57): fix error when adding a comment after a reply obj
- [#61](https://github.com/wandersoncferreira/code-review/pull/61): allow approve PR without feedback comment
- [#63](https://github.com/wandersoncferreira/code-review/pull/63): add naive progress reporter
- [#66](https://github.com/wandersoncferreira/code-review/pull/66): improve error message when personal token is not set
- [#67](https://github.com/wandersoncferreira/code-review/pull/67): improve error messages and `code-review-set-*` functions with proper callbacks
- [#50](https://github.com/wandersoncferreira/code-review/pull/50): add basic Gitlab support
