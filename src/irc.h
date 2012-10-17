/*  Mandatory irc module of GNU Thales.  Copyright (C)
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
#ifndef IRC_H
#define IRC_H
#include <stdbool.h>
#include "conf.h"
struct irc_options {
  const char *host;
  int port;
  char **channels;
};

bool parse_irc_config(struct irc_options *irc_opts, const struct envz *env);

#endif
