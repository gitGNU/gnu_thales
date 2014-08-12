#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
m4_include([modules.af.m4])
AC_PREREQ([2.69])
AC_INIT([GNU Thales], [1.99], [KAction@gnu.org])
AM_INIT_AUTOMAKE([foreign])
GUILE_PROGS
AUTOFRISK_CHECKS
AUTOFRISK_SUMMARY
## If GUILE_SITE is not provided by site-defaults or
## environment, consult guile about it. Usually
## it results in path somewhere in /usr, not
## suitable to user-local install. So we fallback on
## `sed` machinery to provide semi-sane defaults.

if test "x$GUILE_SITE" = x ; then
   GUILE_SITE_DIR
   ## If prefix is specified, then we try to create sensible value
   ## for GUILE_SITE, otherwise just use what we have.
   if test "x$prefix" != xNONE ; then
      ev_datarootdir=`eval echo $datarootdir`
      GUILE_SITE=`echo $GUILE_SITE|sed "s%^/.*/guile%$ev_datarootdir/guile%"`
   fi
fi

if test "x$GUILE_SITE_CCACHE" = x ; then
   GUILE_SITE_CCACHE=`$GUILE -c '(display (%site-ccache-dir))'`
   ## Same, but libdir actually depends on exec_prefix. Annoying.
   if test "x$prefix" != xNONE ; then
      if test "x$exec_prefix" = xNONE ; then
      	 exec_prefix="$prefix"
	 ev_libdir=`eval echo $libdir`
	 exec_prefix=NONE # Does it matter?
      else
         ev_libdir=`eval echo $libdir`
      fi
      GUILE_SITE_CCACHE=`echo $GUILE_SITE_CCACHE| sed "s%^/.*/guile%$ev_libdir/guile%"`
   fi
fi

## But of course, you can specify these directories
## via command line
##
## Courtesy of Mark Mitmer, author of guile-xcb.
AC_ARG_WITH([guile-site-dir],
    [AS_HELP_STRING([--with-guile-site-dir],
    [use the specified installation path for Guile modules])],
        [GUILE_SITE="$withval"], [])

AC_ARG_WITH([guile-site-ccache-dir],
    [AS_HELP_STRING([--with-guile-site-ccache-dir],
    [use the specified installation path for Guile compiled modules])],
        [GUILE_SITE_CCACHE="$withval"], [])
AC_SUBST([GUILE_SITE])
AC_SUBST([GUILE_SITE_CCACHE])

AC_MSG_RESULT(checking for Guile site directory to use ... $GUILE_SITE)
AC_MSG_RESULT(checking for Guile cache site directory to use... $GUILE_SITE_CCACHE)

AC_CONFIG_FILES([
	Makefile
	doc/Makefile
	src/Makefile
	src/thales/Makefile])

AC_OUTPUT
