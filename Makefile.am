## Config file for GNU Automake. 
##
## Copyright (C) 2013, 2014 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-DSV.
## 
## Guile-DSV is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-DSV is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-DSV.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/am/guilec

ACLOCAL_AMFLAGS = -I build-aux

SUBDIRS = am build-aux tests

EXTRA_DIST = \
	srfi/srfi-64.scm		\
	srfi/srfi-64.upstream.scm

SOURCES = dsv.scm

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site
nobase_mod_DATA = $(SOURCES) $(GOBJECTS)

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go

.scm.go:
	$(AM_V_GUILEC)$(guilec_env) $(GUILEC) $(guilec_opts) \
	--output=$@ $<

clean-go:
	@cd ssh; make clean-go
.PHONY: clean-go

# Makefile.am ends here
