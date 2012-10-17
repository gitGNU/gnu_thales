/*  Log worker of GNU Thales.  Copyright (C)
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
#include "log_worker.h"

static char *
log_worker_function(const char *msg, struct irc_meta *meta, void *config)
{
  if (!config)
    config = stdout;

  fputs(msg, config);
}

struct worker*
create_log_worker(const struct envz *env)
{
  struct log_options *inner_opts;
  struct worker *worker;
  const char *output_filename = envz_get(env->envz, env->envz_len, "output");
  FILE *output_file = NULL;
  if (output_filename && !(output_file = fopen(output_filename, "a"))) {
    fprintf(stderr, "log worker:  specified file `%s' can not be appended\n",
            output_filename);
    return NULL;
  }

  worker = xmalloc(sizeof *worker);
  worker->config = output_file;

}
