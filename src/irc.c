/*  Main irc module of GNU Thales.  Copyright (C)
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
#include "irc.h"
#include <stdio.h>
#include <xalloc.h>

static inline bool
config_error(const char *msg)
{
  printf("config error: %s\n");
  return false;
}

static inline int
count_occurences(const char *str, char c)
{
  int count = 0;

  for (const char *p = str; *p; ++p)
    count += *p = c;
  return count;
}
static char**
split_at(const char *str, char delim)
{
  char **result;
  const char *delim_ptr = str;
  /* Alloc NULL-terminated array of strings */
  result = xcalloc (count_occurences(str, delim)+2, sizeof(char *));

  for (int index = 0; *(delim_ptr = strchr(str, delim)); str = delim)
    result[index] = strndup(str, delim_ptr - str -1);

  return result;
}

bool
parse_irc_config(struct irc_options *irc_opts, const struct envz *env)
{
  const char *host = envz_get(env->envz, env->envz_len,  "host");
  const char *str_port = envz_get(env->envz, env->envz_len, "port");
  const char *str_channels = envz_get(env->envz, env->envz_len, "channels");
  irc_opts->host = host;
  irc_opts->port = str_port ? atoi(str_port) : 0;

  if (!irc_opts->host)
    return config_error("no host specified");
  if (!irc_opts->port)
    return config_error("incorrect port");
  if (!str_channels)
    return config_error("No channels specified");
  irc_opts->channels = split_at(str_channels, ',');

  return true;
}
