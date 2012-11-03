/* #include "error.h" Do not include due macro collapse. */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
void
fatal_error (char *msg, ...)
{
  va_list va;
  va_start (va, msg);
  vfprintf (stderr, msg, va);
  exit (EXIT_FAILURE);
}
