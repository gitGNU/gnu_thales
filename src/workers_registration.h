#ifndef WORKERS_REGISTRATION_H
#define WORKERS_REGISTRATION_H
#include "worker.h"
#include "list.h"
#include "conf.h"
struct worker_creator {
  struct worker* (*creator)(const struct envz *);
  const char *type;
  struct list_head list;
};
void init_worker_creators(void);
const struct worker_creator* worker_creator_by_type(const char *type);
#endif
