#ifndef IRC_H
#define IRC_H
#include <stdbool.h>
#include "cmd.h"
#include "list.h"
struct irc_user {
  char *nickname;
  char *realname;
};

bool start_listen_irc(const struct cmd_options *opts,
                      const struct list_head *workers);


#endif
