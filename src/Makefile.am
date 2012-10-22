AM_CFLAGS = -I$(top_srcdir)/lib

bin_PROGRAMS = thales
thales_SOURCES = main.c cmd.h cmd.c conf.h conf.c worker.h \
	log_worker.h log_worker.c irc.h irc.c
thales_LDADD = $(top_builddir)/lib/libgnu.a
