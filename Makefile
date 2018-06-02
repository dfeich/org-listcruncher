EMACS ?= emacs
CASK ?= cask

.PHONY: test org-version cask-update

all: test

test:
	@echo "using $(shell which $(EMACS))"
	$(CASK) exec $(EMACS) --batch -q -l org-listcruncher.el \
           -l test/test-org-listcruncher.el  --eval "(ert-run-tests-batch-and-exit test-order)"

org-version:
	$(CASK) exec $(EMACS) --batch -q --exec "(princ (format \"Org version: %s\" (org-version)) t)"

cask-update:
	$(CASK) install
	$(CASK) update

