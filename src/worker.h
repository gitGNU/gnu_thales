/*  Main modules interface of GNU Thales.  Copyright (C)
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

#ifndef MODULES_H
#define MODULES_H
#include <stddef.h>
#include <envz.h>
#include <argz.h>

#include "list.h"
struct irc_meta {
  int silent_compiler;
};

#define worker_entry(ptr,type,name) (type*)((char *)ptr - offsetof(type,name))
struct worker {
  char *name;
  char *(*process_command)(const char *msg, struct irc_meta *meta,
                           struct worker *self, char *name);
  void (*free_worker)(struct worker *self);
  struct list_head list;
};

#endif
