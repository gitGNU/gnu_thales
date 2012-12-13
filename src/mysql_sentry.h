#ifndef MYSQL_SENTRY_H
#define MYSQL_SENTRY_H

struct mysql_options;
struct sentry;
typedef struct sentry SENTRY;

SENTRY *sentry_initialize(const struct mysql_options *opts);

#endif
