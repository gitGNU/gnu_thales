AM_CPPFLAGS=@GUILE_CFLAGS@
snarfcppopts = $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS) $(AM_CPPFLAGS)
lib_LTLIBRARIES=libguile_message_irc.la
libguile_message_irc_la_SOURCES=message_irc.x message_irc.c
SUFFIXES=.x
.c.x:
	guile-snarf -o $@ $< $(snarfcppopts)
