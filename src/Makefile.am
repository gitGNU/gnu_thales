AM_CFLAGS = -I$(top_srcdir)/lib

bin_PROGRAMS = thales
thales_SOURCES = main.c cmd.h cmd.c worker.h \
	error.h  irc.h irc.c defaults.c defaults.h \
	sentry.h mysql_sentry.h mysql_sentry.c

thales_LDADD = $(top_builddir)/lib/libthales.a
check-syntax:
	$(COMPILE) -o /dev/null -S ${CHK_SOURCES}
