AM_CFLAGS = -I$(top_srcdir)/lib
.DEFAULT_GOAL = maintainer_all
bin_PROGRAMS = thales
thales_SOURCES = main.c cmd.h cmd.c  \
	error.h  irc.h irc.c defaults.c defaults.h \
	sentry.h  sentry.c init_query.h init_query.c \
	function_query.h function_query.c query.h query.c

thales_LDADD = $(top_builddir)/lib/libgnu.a

%.c %.h: %.def %.tpl
	cd $(srcdir) && $(AUTOGEN) $<

prepare: init_query.c query.c
check-syntax:
	$(COMPILE) -o /dev/null -S ${CHK_SOURCES}

.PHONY: prepare
