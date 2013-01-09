#ifndef SENTRY_H
#define SENTRY_H
#include <stdbool.h>
struct db_options;
struct sentry;
typedef struct sentry SENTRY;

SENTRY *sentry_initialize(const struct db_options *opts, const char *server);
void sentry_channel_presence_clear(SENTRY *sentry, const char *channel);
void sentry_channel_presence_add(SENTRY *sentry,  const char *channel,
                                 const char *nickname);
#endif
