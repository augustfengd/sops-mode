EMACS ?= emacs

.PHONY: test
test:
	SOPS_AGE_KEY=AGE-SECRET-KEY-1E0QSX7Y3X2QC4RNQATHWL48K3ZVQA7FLHXAPMFUK3G3U4PPTQ6NSDK995J $(EMACS) --batch -L . -l t/main.el -eval '(ert-run-tests-batch-and-exit)'
