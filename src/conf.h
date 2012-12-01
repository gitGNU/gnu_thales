/*  Configuration file parsing program of GNU Thales.  Copyright (C)
2012 Free Software Foundation, Inc.  This file is part of GNU Thales.

GNU Thales is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

GNU Thales is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.  */

#ifndef CONF_H
#define CONF_H
#include <stdio.h>
#include <stdbool.h>
#include <envz.h>
struct envz {
  char *envz;
  size_t envz_len;
};
#define DECLARE_ENVZ_GET(envz, key) const char *key = \
    envz_get((envz)->envz, ((envz)->envz_len), #key)


typedef void (*module_initializer)(const char *type,
                                  const char *name, const struct envz *env);
FILE* default_config_file(void);
void parse_config(FILE *stream, module_initializer init);
#endif
