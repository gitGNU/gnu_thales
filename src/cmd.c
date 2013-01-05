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
#include <envz.h>
#include "utility.h"

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
  printf_option ("--config, -C", "Override default configuration file");
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

void
parse_cmd_options (struct irc_options *opts, struct config_options *config_opts,
               int argc, char **argv)
{
  const char *optstr = "hdvs:p:C:n:";
  const struct option longopts[] = {
    {"help", no_argument, NULL, 'h'},
    {"server", required_argument, NULL, 's'},
    {"port", required_argument, NULL, 'p'},
    {"debug", no_argument, NULL, 'd'},
    {"version", no_argument, NULL, 'v'},
    {"nick", required_argument, NULL, 'n'},
    {"config", required_argument, NULL, 'C'},
  };
  int val;
  while ((val = getopt_long (argc, argv, optstr, longopts, NULL)) != EOF)
    switch (val)
      {
      case 'h':
	print_help ();
	exit (EXIT_SUCCESS);
      case 'v':
	print_version ();
	exit (EXIT_SUCCESS);
      case 'C':
	config_opts->conf_filename = optarg;
	break;
      case 's':
	opts->server = optarg;
	break;
      case 'p':
	opts->port = (unsigned short int) atoi (optarg); // Port in two byte integral.
	break;
      case 'n':
	opts->nick = optarg;
	break;
      case 'd':
        config_opts->debug = true;
      case '?':
	exit (EXIT_FAILURE);
      }
  opts->channels = argv + optind;
}


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
    if (filenames[index] && (config_file = fopen(filenames[index], "r")))
      return config_file;
  return NULL;
}

static inline void
fill_envz(char **envz, size_t *envz_len, FILE *stream)
{
  char *line = NULL;
  size_t line_len;

  while (getline(&line, &line_len, stream) > 0)
    argz_add(envz, envz_len, line);
  free(line);
}

void
parse_mysql_options(struct mysql_options *opts, FILE *stream)
{
  char *envz = NULL;
  size_t envz_len = 0;

  fill_envz(&envz, &envz_len, stream);

  opts->database = xstrdup_safe(envz_get(envz, envz_len, "database"));
  opts->host = xstrdup_safe(envz_get(envz, envz_len, "host"));
  opts->password = xstrdup_safe(envz_get(envz, envz_len, "password"));
  opts->port = xstrdup_safe(envz_get(envz, envz_len, "port"));
  opts->username = xstrdup_safe(envz_get(envz, envz_len, "username"));

}
