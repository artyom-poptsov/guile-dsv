include $(top_srcdir)/build-aux/am/guile.am

SOURCES = \
	unix-context.scm \
	unix.scm

BUILT_SOURCES = \
	context.scm

EXTRA_DIST += \
	unix.puml

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/dsv/fsm/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/dsv/fsm/

GUILE_SMC_CONTEXT_ARGS = \
	--log-driver file \
	--log-opt "file=context.log" \
	--type functional/char \
	-m "(dsv fsm context)"

GUILE_SMC_UNIX_ARGS = \
	--log-driver file \
	--log-opt "file=unix.log" \
	-L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
	--fsm-name unix-fsm \
	--fsm-module "(dsv fsm unix)" \
	-U "((dsv fsm context) (dsv fsm unix-context))" \
	--debug \
	--validate

GUILE_SMC_UNIX_ENV = GUILE_AUTO_COMPILE=0

if ENABLE_SMC_STANDALONE_MODE
GUILE_SMC_CONTEXT_ARGS += \
	--standalone
GUILE_SMC_UNIX_ARGS += \
	--target=guile-standalone
endif

context.scm:
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc context $(GUILE_SMC_CONTEXT_ARGS) > $@

unix.scm: unix-context.scm unix.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_UNIX_ENV) smc compile $(GUILE_SMC_UNIX_ARGS) unix.puml > $@

# XXX: Automake does not remove those files for some reason.
uninstall-hook:
	rm -f $(DESTDIR)$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)/dsv/fsm/context.scm \
		$(DESTDIR)$(libdir)/guile/3.0/site-ccache/dsv/fsm/context.scm

CLEANFILES += \
	context.scm	\
	unix.log	\
	unix.scm

### Makefile.am ends here.
