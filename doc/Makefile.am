## Makefile.am
##
## Process this file with automake to produce Makefile.in

modhtml = ephemeral_trees.html

doc_DATA = overview.edoc $(modhtml) \
		modules-frame.html overview-summary.html \
		index.html stylesheet.css

overview.edoc: overview.edoc-in
	sed -e 's!%APP_VERSION%!$(PACKAGE_VERSION)!' \
			-e 's!%APP_NAME%!$(PACKAGE_NAME)!' < $< > $@

%.html:$(top_srcdir)/src/%.erl	overview.edoc
	$(ERL) -noshell -run edoc_run application \
			"'$(PACKAGE_NAME)'" "'$(top_srcdir)'" \
			'[{sort_functions,false},{private,true},{todo,true}]'
	for i in `ls *.html`; do sed -e 's!erlang logo!SigScale logo!' \
			-e 's!erlang.org!sigscale.org!' \
			-e 's!erlang.png!sigscale-logo.png!' < $$i > t.html; \
					mv t.html $$i; done

CLEANFILES = overview.edoc edoc-info index.html \
		modules-frame.html overview-summary.html \
		stylesheet.css erlang.png $(modhtml)

