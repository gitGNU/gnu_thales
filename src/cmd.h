/*  Command line parsing program of GNU Thales.  Copyright (C)
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

#ifndef CMD_H
#define CMD_H
#include <stdbool.h>
#include <stdio.h>

struct work_options
{
  const char *server;
  const char *nickname;
  unsigned short port;
  char **channels;
};

FILE *default_config_file (void);
void parse_cmd_options (struct work_options *opts, int argc, char **argv);

#endif
