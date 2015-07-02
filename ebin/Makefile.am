## Makefile.am
##
## Process this file with automake to produce Makefile.in

appdir = $(beamdir)
app_DATA = ephemeral_trees.app
EXTRA_DIST = ephemeral_trees.app

beam_DATA = ephemeral_trees.beam

%.beam:$(top_srcdir)/src/%.erl
	$(ERLC) $(ERLCFLAGS) -I $(top_builddir)/include -I $(top_builddir)/src $<

%.app:   %.app-in
	sed -e 's/%APP_NAME%/$(PACKAGE_NAME)/' \
			-e 's/%APP_VERSION%/$(PACKAGE_VERSION)/' < $< > $@

CLEANFILES = $(beam_DATA) $(app_DATA)

