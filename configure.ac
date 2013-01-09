AC_PREREQ([2.69])
AC_INIT([GNU Thales], [1.91], [thales-misc@gnu.org])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE
# Checks for programs.
AC_PROG_CC_C99
AC_CHECK_PROGS([AUTOGEN], [autogen],
                          [AC_MSG_ERROR([GNU Autogen required, but not found])])
gl_EARLY
gl_INIT
AC_GNU_SOURCE

AC_CHECK_HEADER([libircclient/libircclient.h],[],
        [AC_MSG_ERROR([libircclient header not found])])
AC_CHECK_LIB([ircclient],[irc_create_session],[],
        [AC_MSG_ERROR([libircclient library not found])])
AC_CHECK_HEADER([dbi/dbi.h],[],
        [AC_MSG_ERROR([Libdbi headers not found])])
AC_CHECK_LIB([dbi],[dbi_conn_new],[],
        [AC_MSG_ERROR([Libdbi library not found])])

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.
AC_CONFIG_FILES([Makefile
                                  lib/Makefile
                                  src/Makefile
                                  doc/Makefile])
AC_OUTPUT
