#ifndef CLIENTSET_H
#define CLIENTSET_H
#include "irc.h"


typedef struct ircclient_set ircclient_set;
ircclient_set *ircclient_set_new();
struct irc_client* ircclient_set_find(ircclient_set *set, const char *nickname);
void ircclient_set_remove(ircclient_set *set, const char *nickname);
void ircclient_set_dispose(ircclient_set *set);

#endif
