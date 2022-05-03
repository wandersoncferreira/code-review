# Change log

# Unreleased

# v0.0.7

- [#169](https://github.com/wandersoncferreira/code-review/pull/169): [gitlab] set labels command
- [#175](https://github.com/wandersoncferreira/code-review/pull/175): display PR author in the header. Click/RET to follow
- [#176](https://github.com/wandersoncferreira/code-review/pull/176): [gitlab] fix nested namespaces when it comes from forge
- [#177](https://github.com/wandersoncferreira/code-review/pull/177): [github] support clicability to links
- [#182](https://github.com/wandersoncferreira/code-review/pull/182): fix send single diff comment void section
- [#183](https://github.com/wandersoncferreira/code-review/pull/183): fix cursor moving in unexpected way during comment add/delete
- [#184](https://github.com/wandersoncferreira/code-review/pull/184): [gitlab] remove requirement for feedback to send only comments
- [4ccad8c](https://github.com/wandersoncferreira/code-review/commit/4ccad8cb89ad38075a23023affbac1eb545a99ab): [gitlab] send feedback comment. Sorry commit on master, too tired :/
- [#185](https://github.com/wandersoncferreira/code-review/pull/185): [gitlab] set title
- [#187](https://github.com/wandersoncferreira/code-review/pull/187): [github] feature add - suggestion box bound to `C-c C-s`
- [#188](https://github.com/wandersoncferreira/code-review/pull/188): commands to jump to comments forward and backwards
- [#189](https://github.com/wandersoncferreira/code-review/pull/190): Github: Ability to review a PR without leaving a feedback message
- [#198](https://github.com/wandersoncferreira/code-review/pull/198): [github] clarify docs
- [a7e3d7b](https://github.com/luskwater/code-review/commit/a7e3d7b39d6e442690295767944cb7e42b7a3757): fix minor typo in prompt for unfinished reviews
- [#199](https://github.com/wandersoncferreira/code-review/pull/199): [github] clarify configuration for *GitHub Enterprise*
- [#213](https://github.com/wandersoncferreira/code-review/pull/213): Make auth login marker configurable

# v0.0.6

- [#138](https://github.com/wandersoncferreira/code-review/pull/138): fix bug rendering suggested change block
- [#142](https://github.com/wandersoncferreira/code-review/pull/142): add `code-review-toggle-display-*-comments` functions.
- [#143](https://github.com/wandersoncferreira/code-review/pull/143): add db migration scheme to introduce new columns in the future
- [#148](https://github.com/wandersoncferreira/code-review/pull/148): fix UI heading bug [#145](https://github.com/wandersoncferreira/code-review/issues/145).
- [#150](https://github.com/wandersoncferreira/code-review/pull/150): fix broken indentation with Description heading
- [#155](https://github.com/wandersoncferreira/code-review/pull/155): formal conversion between diff absolute position and diff line number.
- [#156](https://github.com/wandersoncferreira/code-review/pull/156): bitbucket cloud basic workflow integration
- [#157](https://github.com/wandersoncferreira/code-review/pull/157): bitbucket stubs for not implemented feature.
- [#159](https://github.com/wandersoncferreira/code-review/pull/159): place diff under Files Changed section
- [#162](https://github.com/wandersoncferreira/code-review/pull/162): fix obsolete defgeneric to cl-defgeneric.
- [#166](https://github.com/wandersoncferreira/code-review/pull/166): github-related code refactor
- [#167](https://github.com/wandersoncferreira/code-review/pull/167): gitlab-related code refactor
- [#168](https://github.com/wandersoncferreira/code-review/pull/168): bitbucket-related code refactor

# v0.0.5

- [#126](https://github.com/wandersoncferreira/code-review/pull/126): remove `ignore-error` and improve message on `code-review-start`
- [#127](https://github.com/wandersoncferreira/code-review/pull/127): introduce `code-review-github-graphql-host` variable
- [#128](https://github.com/wandersoncferreira/code-review/pull/128): Gitlab support for URLs with subgroups
- [#130](https://github.com/wandersoncferreira/code-review/pull/130): mention user with `C-c @` in comment buffer
- [#131](https://github.com/wandersoncferreira/code-review/pull/131): support Github Enterprise 3.0 by fallback to simpler query in graphql
- [#132](https://github.com/wandersoncferreira/code-review/pull/132): get pr latest SHA even if the branch was already deleted
- [#134](https://github.com/wandersoncferreira/code-review/pull/134): support making single diff comment without review
- [#135](https://github.com/wandersoncferreira/code-review/pull/135): improve package reliability, error messaging, more sane function names

# v0.0.4

- [#96](https://github.com/wandersoncferreira/code-review/pull/96): render comments using HTML to allow displaying images.
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
- [#93](https://github.com/wandersoncferreira/code-review/pull/93): add single top level comment in the PR page without a review attached

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
