EMACS ?= emacs

ELS  = swap-regions.el
ELCS = $(ELS:.el=.elc)

all: $(ELCS) test

%.elc: %.el
	$(EMACS) -Q --batch --eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile $<

test:
	$(EMACS) -Q --batch -L . -l swap-regions-tests.el -f ert-run-tests-batch-and-exit

clean:
	@rm -f *.elc
