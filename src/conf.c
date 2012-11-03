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
#include <envz.h>
#include <ctype.h>

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
  FILE *config_file;

  for (int index = 0; index != countof(filenames); ++index)
    if (filenames[index] && (config_file = fopen(filenames[index], "w")))
      return config_file;
  return NULL;
}


static char *
worddup_between(const char *line, char ldel, char rdel)
{
  char *left = strchr(line, ldel);
  char *right = strchr(line, rdel);

  if (left && right)
    return strndup(left + 1, right - left - 1);
  else
    return NULL;
}
static bool
parse_header(const char *line, char **type, char **name)
{
  free(*type);
  free(*name);

  *type = worddup_between(line, '[', ']');
  *name = worddup_between(line, '{', '}');

  return type && name;
}

static inline void
syntax_error(size_t lineno)
{
  fprintf(stderr, "config_file: syntax error on line %d\n", (int) lineno);
  exit(EXIT_FAILURE);
}
static inline void
initialization_error(const char *type, const char *name)
{
  fprintf(stderr, "module %s: failed to initialize with config `%s'\n",
          type, name);
  exit(EXIT_FAILURE);
}
static inline void
clear_envz(struct envz *env)
{
  free(env->envz);

  env->envz = NULL;
  env->envz_len = 0;
}
static inline bool
all(const char *str, int(*test)(int))
{
  for (const char *p = str; *p; ++p)
    if (!(*test)(*p))
      return false;
  return true;
}
static inline bool
is_comment(const char *line)
{
  return line[0] == '#' || all(line, isspace);
}
static inline void
xinit_module(module_initializer init,  const char *type,
             const char *name, const struct envz *env)
{
  if (!type || !name)
    return;
  (*init)(type, name, env);
}
void
parse_config(FILE *stream, module_initializer init)
{
  char *line, *name, *type;
  size_t line_size = 0;
  struct envz env = {NULL, 0};

  line = name = type = NULL;
  for (size_t lineno = 1; getline(&line, &line_size, stream)!= EOF; lineno++)
    switch (line[0])
      {
      case '[': /* header */
        xinit_module(init, type, name, &env);
        clear_envz(&env);
        if (!parse_header(line, &type, &name))
          syntax_error(lineno);
        break;
      default:
        if (!is_comment(line))
          argz_add(&env.envz, &env.envz_len, line);
      }
  xinit_module(init, type, name, &env);
  clear_envz(&env);
  free(line);
}
