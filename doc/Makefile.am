## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2014-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

info_TEXINFOS = guile-dsv.texi

guile_dsv_TEXINFOS = \
	guile-dsv.texi	\
	fdl.texi

dist_man1_MANS =			\
	dsv.1

## Silent rules.
AM_V_HELP2MAN = $(AM_V_HELP2MAN_$(V))
AM_V_HELP2MAN_ = $(AM_V_HELP2MAN_$(AM_DEFAULT_VERBOSITY))
AM_V_HELP2MAN_0 = @echo "  HELP2MAN" $@;
##
gen_man =						\
	LANGUAGE= $(HELP2MAN) $(HELP2MANFLAGS)

HELP2MANFLAGS = --source=GNU --info-page=$(PACKAGE_TARNAME)

dsv.1: $(top_srcdir)/utils/dsv
	-$(AM_V_HELP2MAN)$(gen_man) --output="$@" `basename "$<"`

## Makefile.am ends here
