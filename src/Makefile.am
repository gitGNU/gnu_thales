AM_CPPFLAGS=@GUILE_CFLAGS@
snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(AM_CPPFLAGS)
lib_LTLIBRARIES=libguile-ircclient.la
# Guile-snarf complains if I split init.c and cmd.c
# So now init.c include cmd.c
libguile_ircclient_la_SOURCES=init.x init.c
SUFFIXES=.x
.c.x:
	guile-snarf -o $@ $< $(snarfcppopts)
SUBDIRS=doc
