EMACS       := emacs
FIND        := find

EXTRAS      := $(PWD)/extras
SRC         := $(PWD)/src

LOGO_ANS    := $(EXTRAS)/ansi/logo.ans
LOGO_PNG    := $(PWD)/logo.png

EMACSFLAGS  := --batch -q --no-site-file
EMACSCMD     = $(EMACS) $(EMACSFLAGS)


all: clean compile


cask-%:
	$(CASK) exec $(MAKE) $(*)

clean:
	$(FIND) $(SRC) -iname "*.elc" -delete

compile:
	$(EMACSCMD) --eval "(byte-recompile-directory \"$(SRC)\" 0)"

install: compile
	$(EMACSCMD) \
		--eval "(require 'package)" --eval "(package-install-file \"$(SRC)\")"

logo.png:
	$(EMACSCMD) \
		-L $(SRC) --eval "(require 'ansilove)" \
		--eval "(ansilove--convert-file-to-png \"$(LOGO_ANS)\" \"$(LOGO_PNG)\")"

clean-logo:
	if [ -f logo.png ] ; then rm logo.png ; fi

logo: clean-logo logo.png
