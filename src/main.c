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
#include "cmd.h"
#include "conf.h"
#include "list.h"
#include "error.h"
#include  "workers_registration.h"
static LIST_HEAD(workers);
static void
module_initialization_dispatcher(const char *type,
                                 const char *name, const struct envz *env)
{
  const struct worker_creator *cr = worker_creator_by_type(type);
  struct worker *worker;

  if (!cr)
    fatal_error("No module of type `%s' registered", type);

  worker = (*cr->creator)(env);
  if (!worker)
    fatal_error("Failed to initiailize modude of type %s with name %s",
                type, name);

  list_add(&worker->list, &workers);
}
int
main(int argc, char **argv)
{
  struct cmd_options opts = {
    .conf_filename = NULL,
    .host = NULL,
    .port = 0,
    .channels = NULL
  };
  FILE *config_file;

  parse_cmdopts(&opts, argc, argv);
  config_file = opts.conf_filename ?
    fopen(opts.conf_filename, "r")
    : default_config_file();
  if (!config_file)
    fatal_error("failed to open config file");
  init_worker_creators();
  parse_config(config_file, &module_initialization_dispatcher);

  return 0;
}
