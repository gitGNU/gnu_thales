AM_CFLAGS = -I$(top_srcdir)/lib

bin_PROGRAMS = thales
thales_SOURCES = main.c cmd.h cmd.c conf.h conf.c worker.h \
	log_worker.h log_worker.c error.h workers_registration.c \
	workers_registration.h

thales_LDADD = $(top_builddir)/lib/libthales.a
