#ifndef IRC_H
#define IRC_H
#include <stdbool.h>
struct work_options;
struct ircclient
{
  char *nickname;
  char *realname;
  char *server;
  char ipv4[8];			/* 8 hex digits */
  bool ready;
};

bool client_start_listen (const struct work_options *opts);
void client_stop_listen (void);


#endif
