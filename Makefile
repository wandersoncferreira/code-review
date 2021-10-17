.PHONY: test

test:
	cask install
	cask exec buttercup-run-discover -L test/
