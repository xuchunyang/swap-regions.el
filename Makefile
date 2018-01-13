EMACS ?= emacs

ELS  = swap-regions.el
ELCS = $(ELS:.el=.elc)

all: $(ELCS)

%.elc: %.el
	$(EMACS) -Q --batch --eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile $<
