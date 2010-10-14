dnl -*- Autoconf -*-

AC_INIT(tekuti, 0.2.0)
AC_CONFIG_SRCDIR(src/tekuti)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability])
AM_SILENT_RULES([yes])

GUILE_PROGS

if $GUILE_TOOLS | grep -q compile; then
   # everything cool
   true
else
   AC_MSG_ERROR([Guile 2.0 required.])
fi

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([env], [chmod +x env])

AC_OUTPUT
