# Change log

# Unreleased

- [#96](https://github.com/wandersoncferreira/code-review/pull/96/files): render comments using HTML to allow displaying images.
- [#98](https://github.com/wandersoncferreira/code-review/pull/98): add CI status on commits section. Github only. Demo [here](https://github.com/wandersoncferreira/code-review/pull/98).
- [#101](https://github.com/wandersoncferreira/code-review/pull/101): fix bug with Gitlab rendering comments with HTML.
- [#102](https://github.com/wandersoncferreira/code-review/pull/102): fix bug with Gitlab sending comments and rendering HTML details.
- [#103](https://github.com/wandersoncferreira/code-review/pull/103): when PR does not have CI setup do not show any symbol
- [#104](https://github.com/wandersoncferreira/code-review/pull/104): full reload using `G`
- [#105](https://github.com/wandersoncferreira/code-review/pull/105): restrict approval to PRs with bad CI state. See docs of `code-review-always-restrict-approval?` too.
- [#111](https://github.com/wandersoncferreira/code-review/pull/111): fix comments placed in wrong position. Needs better solution to compute amount of HTML lines in the buffer.
- [#113](https://github.com/wandersoncferreira/code-review/pull/113): order conversation comments section by createdAt timestamp.
- [#114](https://github.com/wandersoncferreira/code-review/pull/114): improve code structure.
- [#118](https://github.com/wandersoncferreira/code-review/pull/118): add documentation to enterprise users
- [#119](https://github.com/wandersoncferreira/code-review/pull/119): add feedback msg if no milestone is found
- [#120](https://github.com/wandersoncferreira/code-review/pull/120): set description field using text not html
- [#121](https://github.com/wandersoncferreira/code-review/pull/121): do not set `k` as default keybinding to delete comments

# v0.0.3

- [#68](https://github.com/wandersoncferreira/code-review/pull/68): add files changed heading before diff
- [#74](https://github.com/wandersoncferreira/code-review/pull/74): fix `code-review-forge-pr-at-point` command
- [#75](https://github.com/wandersoncferreira/code-review/pull/75): fix bug related to MELPA not delivering graphql files by default. Special thanks to [@mplanchard](https://github.com/mplanchard)
- [#73](https://github.com/wandersoncferreira/code-review/pull/73): add reviewers section and `set reviewers` command
- [#80](https://github.com/wandersoncferreira/code-review/pull/80): make transient keys follow Forge convention of key spacing
- [#49](https://github.com/wandersoncferreira/code-review/pull/46): promote comment to new issue. Github only.
- [#85](https://github.com/wandersoncferreira/code-review/pull/85): remove comment and feedback section with `k`
- [#86](https://github.com/wandersoncferreira/code-review/pull/86): allow users to define function to open Code Review buffer.
- [#90](https://github.com/wandersoncferreira/code-review/pull/90): support visit binary files on Dired (`RET`) or Remote (`C-c C-v`)
- [#93](https://github.com/wandersoncferreira/code-review/pull/93/files): add single top level comment in the PR page without a review attached


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
