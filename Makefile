.PHONY: test

test:
	cask upgrade
	cask install
	cask exec buttercup -L test/
