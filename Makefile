EMACS ?= emacs

# only use KEG if it is available in the PATH or the location is explicitely
# given
ifndef KEG
  ifneq (, $(shell which keg))
  KEGEXEC = keg exec
  KEG = keg
  endif
else
  KEGEXEC = $(KEG) exec
endif

ifdef TESTNAME
testcmd = (ert \"$(TESTNAME)\")
else
testcmd = (ert t)
endif

.PHONY: test debug clean

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<


all: test

compile: org-listcruncher.elc
	@echo "Byte compiling $<"
	$(KEGEXEC) $(EMACS) --batch \
              --eval "(byte-compile-file \"org-listcruncher.el\")"

test: org-listcruncher.elc
	@echo "Emacs binary at $(shell which $(EMACS))"
	$(KEGEXEC) $(EMACS) --batch \
                            -l org-listcruncher.elc \
                            -l test/test-org-listcruncher.el \
             --eval "(princ (format \"Emacs version: %s\n\" (emacs-version)) t)" \
	     --eval "(princ (format \"Org version: %s\n\" (org-version)) t)" \
             --eval "(ert-run-tests-batch-and-exit test-order)"

debug:
	$(KEGEXEC) $(EMACS) -q \
                            -l org-listcruncher.el \
                            -l test/test-org-listcruncher.el \
                            --eval "(progn (setq occ-no-cleanup t)$(testcmd))"

clean:
	rm -f org-listcruncher.elc
	@if test x"${KEGEXEC}" != x; then \
	  $(KEG) clean; \
        fi
