.PHONY: test

test:
	cask upgrade-cask
	cask install
	cask exec buttercup -L test/
