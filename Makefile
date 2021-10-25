# Determine this makefile's path.
# Be sure to place this BEFORE `include` directives, if any.
# Source: https://stackoverflow.com/a/27132934/323874
THIS_FILE := $(lastword $(MAKEFILE_LIST))

EMACS=emacs
CASK ?= cask

INIT_PACKAGE_EL="(progn  \
  (require 'package)  \
  (push '(\"melpa\" . \"http://melpa.org/packages/\") package-archives)  \
  (package-initialize)  \
  (unless package-archive-contents \
     (package-refresh-contents)))"

PROJECT_FILES=`${CASK} files`

clean:
	${CASK} clean-elc

install:
	${CASK} install

lint:
	${CASK} exec $(EMACS) -Q --batch  \
	                      --eval $(INIT_PACKAGE_EL)  \
	                      -l "package-lint.el"  \
	                      -f "package-lint-batch-and-exit"  \
	                      ${PROJECT_FILES}

checkdoc:
	${CASK} exec $(EMACS) -Q --batch  \
	                      --eval $(INIT_PACKAGE_EL)  \
	                      -l "dev/build-utils.el"  \
	                      --eval '(flycheck/batch-checkdoc ".")'

build :
	${CASK} build

test: build
	    ${CASK} exec buttercup -L test/

.PHONY: clean install lint checkdoc build test
