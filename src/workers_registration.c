#include "workers_registration.h"
#include "log_worker.h"
#include "list.h"
#include "xalloc.h"
static LIST_HEAD (worker_creators_list);

#define add_worker_creator(TYPE, CREATOR) \
  do {                                                          \
  struct worker_creator *_node = xmalloc(sizeof *_node);   \
  _node->type = (TYPE);                                         \
  _node->creator = (CREATOR);                                   \
  list_add(&_node->list, &worker_creators_list);                   \
  } while (0)


void
init_worker_creators (void)
{
  add_worker_creator ("log", create_log_worker);
}

const struct worker_creator *
worker_creator_by_type (const char *type)
{
  struct list_head *ptr;
  list_for_each (ptr, &worker_creators_list)
  {
    struct worker_creator *node =
      list_entry (ptr, struct worker_creator, list);
    if (!strcmp (type, node->type))
      return node;
  }
  return NULL;
}
