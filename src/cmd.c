/*  Command line parsing program of GNU Thales.  Copyright (C)
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
#include "cmd.h"
#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static void
printf_option(const char *option, const char *description)
{
  printf("  %-20s%-20s\n", option, description);
}
static void
print_help(void)
{
  printf("Usage: thales [options]\n");
  printf_option("--help, -h", "Display this information");
  printf_option("--version, -v", "Display thales version");
  printf_option("--debug, -d", "Enable output of debug information");
  printf_option("--config, -C", "Override default configuration file");
}

static void
print_version(void)
{
  puts(PACKAGE_STRING);
  puts("Copyright (C) 2012 Free Software Foundation, Inc.");
  puts("This is free software; see the source for copying conditions.  There is NO");
  puts("warranty; not even for MERCHANTABILITY "
       "or FITNESS FOR A PARTICULAR PURPOSE.");
}

void
parse_cmdopts(struct cmd_options *opts, int argc, char **argv)
{
  int val;
  const char *optstr = "hvC:";
  struct option longopts[] = {
    {"help", no_argument, NULL, 'h'},
    {"version", no_argument, NULL, 'v'},
    {"config", required_argument, NULL, 'C'},
  };
  while ((val = getopt_long(argc, argv, optstr, longopts, NULL))!= EOF)
    switch (val)
      {
      case 'h':
        print_help();
        exit(EXIT_SUCCESS);
      case 'v':
        print_version();
        exit(EXIT_SUCCESS);
      case 'c':
        opts->conf_filename = optarg;
        break;
      case '?':
        exit(EXIT_FAILURE);
      }
}