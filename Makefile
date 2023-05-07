EMACS ?=	emacs

ELISP_SRCS=	selrgn.el
ELISP_OBJS=	$(ELISP_SRCS:.el=.elc)

.PHONY: all
all: build

.PHONY: build
build: $(ELISP_OBJS)

.PHONY: test
test: build
	$(EMACS) -Q --batch -L . -l selrgn-tests.el -f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	-rm -f $(ELISP_OBJS)

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q -batch -f batch-byte-compile $<
