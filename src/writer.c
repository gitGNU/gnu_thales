#include <xalloc.h>
#include <stdbool.h>
#include "cmd.h"

#define writer_printf(context, fmt, ...) printf("%s/%s:%d\t" fmt, \
                                                __func__+7, /* Remove `writer_` prefix */ \
                                                context->server, context->port, __VA_ARGS__)
void
writer_channel_presence_clear(struct work_options *context, const char *channel)
{
  writer_printf(context, "%s\n", channel);
}

void
writer_channel_presence_add(struct work_options *context, const char *channel,
                            const char *nickname)
{
  writer_printf(context, "%s\t%s\n", channel, nickname);
}

void
writer_channel_presence_remove(struct work_options *context, const char *channel,
                                    const char *nickname)
{
  writer_printf(context, "%s\t%s\n", channel, nickname);
}
void
writer_channel_topic_update(struct work_options *context, const char *channel,
                                 const char *nickname, const char *new_topic)
{
  writer_printf(context, "%s\t%s\t%s\n", channel, nickname, new_topic);
}
void
writer_nick_change(struct work_options *context, const char *oldnick, const char *newnick)
{
  writer_printf(context, "%s\t%s\n", oldnick, newnick);
}
void
writer_nick_quit(struct work_options *context, const char *nickname)
{
  writer_printf(context, "%s\n", nickname);
}
