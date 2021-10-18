[![GPL v3](https://img.shields.io/badge/license-GPL_v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

# Code Review

Package to help you perform code reviews from your VC provider. Currently
supports only Github.


## List of TODOs before launch

- [ ] Edit main comment
- [ ] Edit regular comment
- [ ] Delete regular comment
- [ ] LGTM! fast forward
- [ ] Display the main body comments in the PR [Blocker]
- [ ] Review transient to regular comment usability a priority
- [ ] Move more functions to `utils` to ease maintenance
- [ ] Another look at the code to avoid stateful values
  - consider using a database like forge does?
- [ ] BIG work on faces to distinguish several scenarios! [BLOCKER]!
  - [x] Some initial improvements on header and main sections
  - [ ] Improve comment sections
  - [ ] Improve comment sections outdated
- [ ] README examples and documentation
- [ ] Change GITHUB specific code to use `cl-generic` [BLOCKER]!
- [ ] Provide docs about requirements to add other forges
- [ ] Fix file dependencies
- [ ] Write tests to at least `utils` logic
- [ ] Refactor many `a-list` access pattern
- [ ] Group styling properties calls on `code-review-section`
- [x] Fill header
  - [x] Fix state style
  - [x] Get Refs
  - [x] Get Milestones
  - [x] Get Labels
  - [x] Get Assignee
  - [x] Get suggested-reviewers
  - [x] Get project
- [ ] Show others deliberations (nice!)
- [x] Implement list of Commits in the PR [BLOCKER]
- [ ] Review a commit on RET [BLOCKER]
- [ ] How to inform the two Magit functions overwrites?
  - are there any standard practice in emacs to make this more explicit?
- [ ] Organize `utils` file
