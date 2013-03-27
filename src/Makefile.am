AM_CPPFLAGS = -I$(top_srcdir)/lib
AM_CFLAGS= -Wno-unused-parameter
bin_PROGRAMS = thales
thales_SOURCES = main.c cmd.h cmd.c  \
	error.h  irc.h irc.c writer.c writer.h

thales_LDADD = $(top_builddir)/lib/libgnu.a

%.c %.h: %.def %.tpl
	cd $(srcdir) && $(AUTOGEN) $<

prepare:
check-syntax: prepare
	$(COMPILE) -o /dev/null -S ${CHK_SOURCES}
GTAGS: prepare
	gtags -I .
.PHONY: prepare GTAGS check-syntax
