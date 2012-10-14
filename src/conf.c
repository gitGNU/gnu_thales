/*  Configuration file parsing program of GNU Thales.  Copyright (C)
2012 Free Software Foundation, Inc.  This file is part of GNU Thales.

GNU Thales is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

GNU Make is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.  */
#include <config.h>
#include "conf.h"
#include <stdbool.h>
#include <stdlib.h>

#define countof(x) sizeof(x)/sizeof(0[x])
FILE*
default_config_file(void)
{
  /* Have to replace hardcoded pathes to
     Automake generated */
  const char *filenames[] ={
    getenv("THALES_CONFIG"),
    "~/.thales",
    "/etc/thales"
  };
  int index;
  FILE *config_file;
  for (index = 0; index != countof(filenames); ++index)
    if (filenames[index] && (config_file = fopen(filenames[index], "w")))
      return config_file;
  return NULL;
}
