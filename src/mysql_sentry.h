#ifndef MYSQL_SENTRY_H
#define MYSQL_SENTRY_H
#include <stdbool.h>
struct mysql_options;
struct sentry;
typedef struct sentry SENTRY;

SENTRY *sentry_initialize(const struct mysql_options *opts);
bool sentry_channel_presence_clear(SENTRY *sentry, const char *channel);
bool sentry_channel_presence_add(SENTRY *sentry,  const char *channel,
                                 const char *nickname);

bool sentry_update_presence(SENTRY *sentry,
                            const char *nickname, const char *channel, bool online);


#endif
