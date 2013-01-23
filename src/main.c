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
#include "error.h"
#include "irc.h"
#include "sentry.h"

int
main (int argc, char **argv)
{
  struct irc_options irc_opts = { 0 };
  struct mysql_options mysql_opts = { 0 };
  struct config_options config_opts = { 0 };


  parse_cmd_options (&irc_opts, &config_opts, argc, argv);
  FILE *config_file = config_opts.conf_filename ? fopen (config_opts.conf_filename, "r")
    : default_config_file ();

  if (!config_file)
    fatal ("failed to open config file");

  parse_mysql_options(&mysql_opts, config_file);

SENTRY *sentry = sentry_initialize(&mysql_opts, irc_opts.server);
#warning "mysql is not used"
/* if (!sentry) */
/*   fatal("failed to connect to database"); */

  start_listen_irc(&irc_opts, sentry);
  return 0;
}
