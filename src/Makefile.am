AM_CFLAGS = -I$(top_srcdir)/lib

bin_PROGRAMS = thales
thales_SOURCES = main.c cmd.h cmd.c  \
	error.h  irc.h irc.c defaults.c defaults.h \
	sentry.h  sentry.c init_query.h init_query.c \
	function_query.h function_query.c
thales_LDADD = $(top_builddir)/lib/libthales.a

check-syntax:
	$(COMPILE) -o /dev/null -S ${CHK_SOURCES}
