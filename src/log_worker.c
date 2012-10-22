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
#include <stdlib.h>

#include "xalloc.h"

struct log_worker {
  struct worker worker;
  FILE  *out_stream;
};

static char *
log_worker_process_message(const char *msg, struct irc_meta *meta,
                           struct worker *embed, const char *name)
{
  struct log_worker *logger = worker_entry(embed, struct log_worker, worker);
  FILE *stream = logger->out_stream ? logger->out_stream : stdout;

  fprintf(stream, "[%s]: %s\n", name, msg);
  return NULL;
}

static void
log_worker_free_worker(struct worker *embed)
{
  struct log_worker *logger = worker_entry(embed, struct log_worker, worker);

  if (logger->out_stream)
    fclose(logger->out_stream);
  free(logger);
}

struct worker*
create_log_worker(const struct envz *env)
{
  struct log_worker *logger;
  const char *output_filename = envz_get(env->envz, env->envz_len, "output");
  FILE *out_stream = NULL;
  if (output_filename && !(out_stream = fopen(output_filename, "a"))) {
    fprintf(stderr, "log worker:  specified file `%s' can not be appended\n",
            output_filename);
    return NULL;
  }

  logger = xmalloc(sizeof *logger);
  logger->out_stream = out_stream;
  return &logger->worker;
}
