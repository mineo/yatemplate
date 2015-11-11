INSTALL_DEPS = "(progn (package-initialize) (package-install 'test-simple) (package-install 'yasnippet))"

.PHONY: test

test:
	emacs --batch --eval $(INSTALL_DEPS) -l yatemplate-tests.el
