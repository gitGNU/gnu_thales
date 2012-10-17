#ifndef WORKERS_REGISTRATION_H
#define WORKERS_REGISTRATION_H
#include "worker.h"
typedef struct worker* (*worker_creator)(const struct envz *);
void init_worker_creators(void);
worker_creator worker_creator_by_type(const char *type);

typedef struct team team;
team* team_alloc();
void team_add_worker(team *, const char *name, struct worker *);
struct worker *team_get_worker(const char *name);
void team_dealloc(team *)

#endif
