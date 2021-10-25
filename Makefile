.PHONY: test

test:
	cask install
	cask exec buttercup -L test/
