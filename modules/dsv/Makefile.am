## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2015 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

include $(top_srcdir)/build-aux/am/guile.am

SUBDIRS = . cli

SOURCES = \
	version.scm		\
	config.scm		\
	rfc4180.scm		\
	unix.scm		\
	parser.scm		\
	builder.scm		\
	table.scm		\
	table-preset.scm	\
	common.scm

EXTRA_DIST += \
	config.scm.in		\
	version.scm.in

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/dsv
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/dsv

substitute = \
	sed -e "s,[@]DATA_DIR[@],$(pkgdatadir),g"

version_substitute = \
	sed -e 's,[@]VERSION[@],$(PACKAGE_VERSION),g'

version.scm: version.scm.in
	@echo "  GEN      $@"
	@$(version_substitute) "$^" > "$@"

config.scm: config.scm.in
	@echo "  GEN      $@"
	@$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

CLEANFILES += \
	config.scm		\
	version.scm

# Makefile.am ends here
