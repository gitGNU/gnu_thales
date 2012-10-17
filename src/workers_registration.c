#include "workers_registration.h"
struct registered_worker_creator {
  worker_creator creator;
  const char *typename;
};
