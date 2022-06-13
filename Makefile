# This file is part of emacs-ansilove.

# emacs-ansilove is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 3.

# emacs-ansilove is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with emacs-ansilove.  If not, see <https://www.gnu.org/licenses/>.

# Copyright (c) 2022, Maciej Barć <xgqt@riseup.net>
# Licensed under the GNU GPL v3 License
# SPDX-License-Identifier: GPL-3.0-only


EMACS       := emacs
FIND        := find

SRCDIR      := $(PWD)/src

EMACSFLAGS  := --batch -q --no-site-file
EMACSCMD     = $(EMACS) $(EMACSFLAGS)


all: clean compile


cask-%:
	$(CASK) exec $(MAKE) $(*)

clean:
	$(FIND) $(SRCDIR) -iname "*.elc" -delete

compile:
	$(EMACSCMD) --eval "(byte-recompile-directory \"$(SRCDIR)\" 0)"

install: compile
	$(EMACSCMD) \
		--eval "(require 'package)" \
		--eval "(package-install-file \"$(SRCDIR)\")"
