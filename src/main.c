/*  Main program of GNU Thales.  Copyright (C)
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

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cmd.h"
#include "conf.h"
#include "list.h"
#include "error.h"
#include  "workers_registration.h"
#include "irc.h"

int
main (int argc, char **argv)
{
  struct cmd_options cmd_opts = { 0, };
  struct mysql_options mysql_opts = { 0, };
  FILE *config_file;

  parse_cmdopts (&cmd_opts, argc, argv);
  config_file = cmd_opts.conf_filename ? fopen (opts.conf_filename, "r")
    : default_config_file ();

  if (!config_file)
    fatal ("failed to open config file");

  parse_mysql_options(&mysql_opts);
  start_listen_irc (&cmd_opts, &workers);
  return 0;
}
