# -*- Makefile -*-

# "make check" by default runs non-expensive tests.  To run all tests,
# use "make check SELECTOR=t".

EMACS = emacs -Q -batch -L .
SRCS = javaimp-util.el javaimp-gradle.el javaimp-maven.el javaimp-parse.el javaimp.el javaimp-tests.el
TESTS = $(wildcard tests/*.el)
OBJS = $(SRCS:.el=.elc) $(TESTS:.el=.elc)
SELECTOR ?= (not (tag :expensive))

.PHONY: all check test clean

%.elc: %.el
	${EMACS} -f batch-byte-compile $^

all: $(OBJS)

check test: all
	${EMACS} \
	  $(addprefix -l ,$(OBJS)) \
	  -eval '(ert-run-tests-batch-and-exit (quote ${SELECTOR}))'

clean:
	$(RM) -f $(OBJS)
