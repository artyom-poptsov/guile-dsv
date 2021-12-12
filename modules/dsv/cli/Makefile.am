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

include $(top_srcdir)/build-aux/am/guilec

SOURCES = \
	config.scm	\
	common.scm	\
	table.scm	\
	preset.scm

EXTRA_DIST = \
	config.scm.in

GOBJECTS = $(SOURCES:%.scm=%.go)
moddir=$(guilesitedir)/dsv/cli
nobase_dist_mod_DATA = $(SOURCES)

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/dsv/cli
nobase_nodist_ccache_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_dsv_install_go_files = install-nobase_dist_ccacheDATA
$(guile_dsv_install_go_files): install-nobase_dist_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat

substitute = \
	sed -e "s,[@]DATA_DIR[@],$(pkgdatadir),g"

config.scm: config.scm.in
	@echo "  GEN      $@"
	@$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

guilec_opts = --load-path=$(abs_top_srcdir)/modules/
.scm.go:
	$(AM_V_GUILEC)$(guilec_env) $(GUILEC) $(guilec_opts) \
	--output=$@ $<

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES = \
	$(GOBJECTS) \
	config.scm


# Makefile.am ends here
