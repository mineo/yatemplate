CASK = cask
EMACSBATCH = $(CASK) exec emacs -Q --batch -L .

.PHONY: setup
setup:
	$(CASK) install
	$(CASK) update

.PHONY: test
test:
	$(EMACSBATCH) -l yatemplate-tests.el
