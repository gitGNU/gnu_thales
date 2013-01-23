#ifndef IRC_H
#define IRC_H
#include <stdbool.h>
#include "cmd.h"
#include "sentry.h"
struct irc_user {
  char *nickname;
  char *realname;
};

bool start_listen_irc(const struct irc_options *opts, SENTRY *sentry);



#endif
