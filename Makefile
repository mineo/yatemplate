CASK = cask

.PHONY: setup
setup:
	$(CASK) install
	$(CASK) update

.PHONY: test
test:
	$(CASK) exec buttercup -L .
