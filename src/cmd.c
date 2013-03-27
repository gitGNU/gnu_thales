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

#include <config.h>
#include "cmd.h"
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "error.h"

static const char *default_server = "irc.freenode.net";
static const unsigned short default_port = 6667;
static const char *default_nickname ="__GNU_Thales__";

static void
printf_option (const char *option, const char *description)
{
  printf ("  %-20s%-20s\n", option, description);
}

static void
print_help (void)
{
  printf ("Usage: thales [options] channel [channels]\n");
  printf_option ("--help, -h", "Display this information");
  printf_option ("--version, -v", "Display thales version");
  printf_option ("--debug, -d", "Enable output of debug information");
  printf_option ("--config, -C", "Override default work_options file");
  printf_option ("--server, -s", "Irc server to connect");
  printf_option ("--port, -p", "Port to connect");
  printf_option ("--nick, -n", "Nickname to login with");
}

static void
print_version (void)
{
  puts (PACKAGE_STRING);
  puts ("Copyright (C) 2012 Free Software Foundation, Inc.");
  puts
    ("This is free software; see the source for copying conditions.  There is NO");
  puts ("warranty; not even for MERCHANTABILITY "
	"or FITNESS FOR A PARTICULAR PURPOSE.");
}

static void
cmd_set_defaults (struct work_options *opts)
{
  opts->nickname = opts->nickname ? opts->nickname : default_nickname;
  opts->server = opts->server ? opts->server : default_server;
  opts->port = opts->port ? opts->port : default_port;
}

void
parse_cmd_options (struct work_options *opts,  int argc, char **argv)
{
  const char *optstr = "hvs:p:C:n:";
  const struct option longopts[] = {
    {"help", no_argument, NULL, 'h'},
    {"server", required_argument, NULL, 's'},
    {"port", required_argument, NULL, 'p'},
    {"version", no_argument, NULL, 'v'},
    {"nick", required_argument, NULL, 'n'},
    {NULL, 0, NULL, 0}
  };
  int val;
  while ((val = getopt_long (argc, argv, optstr, longopts, NULL)) != EOF)
    switch (val)
      {
      case 'h':
	print_help ();
	exit (EXIT_SUCCESS);
        break;
      case 'v':
	print_version ();
	exit (EXIT_SUCCESS);
	break;
      case 's':
	opts->server = optarg;
	break;
      case 'p':
	opts->port = (unsigned short int) atoi (optarg);	// Port in two byte integral.
	break;
      case 'n':
	opts->nickname = optarg;
	break;
      case '?':
      default:
	print_help ();
	exit (EXIT_FAILURE);
      }
  cmd_set_defaults (opts);
  opts->channels = argv + optind;

  if (!*opts->channels)
    {				/* No channels specified */
      print_help ();
      exit (EXIT_FAILURE);
    }
}
