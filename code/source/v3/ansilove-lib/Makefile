EMACS                   := emacs
FIND                    := find
RM                      := rm -f -r

PWD                     := $(shell pwd)

SRCDIR                  := $(PWD)/src/main/elisp
TESTSDIR                := $(PWD)/src/test/elisp

CACHEDIR                := $(PWD)/.cache
IDIR                    := $(CACHEDIR)/image
BINIDR                  := $(IDIR)/usr/bin
EMACSIDIR               := $(IDIR)/usr/share/emacs/site-lisp

EMACSFLAGS              := --batch -q --no-site-file
EMACSCMD                 = $(EMACS) $(EMACSFLAGS)
TESTFLAGS               := -L $(PWD) --traceback full
TESTCMD                  = $(BUTTERCUP) $(TESTFLAGS)

.PHONY: all
all:
	$(MAKE) clean
	$(MAKE) compile

clean-%:
	$(FIND) $(SRCDIR)/$(*) -iname "*.elc" -delete

.PHONY: clean
clean:
	$(MAKE) clean-ansilove
	if [ -d $(CACHEDIR) ] ; then $(RM) $(CACHEDIR) ; fi

compile-%:
	$(EMACSCMD) --directory $(SRCDIR)/$(*) \
		--eval "(byte-recompile-directory \"$(SRCDIR)/$(*)\" 0)"

.PHONY: compile
compile:
	$(MAKE) compile-ansilove

test-%: compile-%
	$(TESTCMD) --directory $(TESTSDIR)/$(*) \
		-L $(SRCDIR)/$(*) -L $(TESTSDIR)/$(*)

install-%: compile-%
	$(EMACSCMD) --eval "(require 'package)" \
		--eval "(package-install-file \"$(SRCDIR)/$(*)\")"

.PHONY: install
install:
	$(MAKE) install-ansilove
