#ifndef MYSQL_SENTRY_H
#define MYSQL_SENTRY_H

#include "sentry.h"
#include "conf.h"

#define sentry_entry(ptr,type,name) (type*)((char *)ptr - offsetof(type,name))
struct sentry* create_mysql_sentry(const struct envz *);

#endif
