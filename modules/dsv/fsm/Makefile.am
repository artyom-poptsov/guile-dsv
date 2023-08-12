include $(top_srcdir)/build-aux/am/guile.am

SOURCES = \
	dsv-context.scm \
	unix.scm \
	rfc4180.scm

BUILT_SOURCES = \
	context.scm

INSTALL += \
	context.scm

EXTRA_DIST += \
	unix.puml \
	unix.scm \
	rfc4180.puml \
	rfc4180.scm

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
	-U "((dsv fsm context) (dsv fsm dsv-context))" \
	--debug \
	--validate

GUILE_SMC_ENV = GUILE_AUTO_COMPILE=0

GUILE_SMC_RFC4180_ARGS = \
	--log-driver file \
	--log-opt "file=rfc4180.log" \
	-L $(abs_top_srcdir)/modules:$(abs_top_builddir)/modules \
	--fsm-name rfc4180-fsm \
	--fsm-module "(dsv fsm rfc4180)" \
	-U "((dsv fsm context) (dsv fsm dsv-context))" \
	--debug \
	--validate

if ENABLE_SMC_STANDALONE_MODE
GUILE_SMC_CONTEXT_ARGS += \
	--standalone
GUILE_SMC_UNIX_ARGS += \
	--target=guile-standalone
endif

context.scm:
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc context $(GUILE_SMC_CONTEXT_ARGS) > $@

unix.scm: context.scm dsv-context.scm unix.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_UNIX_ARGS) unix.puml > $@

rfc4180.scm: context.scm dsv-context.scm rfc4180.puml
	@echo "  SMC      $@"
	@$(GUILE_SMC_ENV) smc compile $(GUILE_SMC_RFC4180_ARGS) rfc4180.puml > $@


# XXX: Automake does not remove those files for some reason.
uninstall-hook:
	rm -f $(DESTDIR)$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)/dsv/fsm/context.scm \
		$(DESTDIR)$(libdir)/guile/3.0/site-ccache/dsv/fsm/context.scm

CLEANFILES += \
	context.scm	\
	unix.log	\
	unix.scm	\
	rfc4180.log	\
	rfc4180.scm

### Makefile.am ends here.
