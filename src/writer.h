#ifndef WRITER_H
#define WRITER_H
#include <stdbool.h>
struct work_options;

void writer_channel_presence_clear(const struct work_options *context, const char *channel);
void writer_channel_presence_add(const struct work_options *context,  const char *channel,
                                 const char *nickname);
void writer_channel_presence_remove(const struct work_options *context, const char *channel,
                                    const char *nickname);
void writer_channel_topic_update(const struct work_options *context, const char *channel,
                                 const char *nickname, const char *new_topic);
void writer_nick_change(const struct work_options *context, const char *oldnick, const char *newnick);
void writer_nick_quit(const struct work_options *context, const char *nickname);

#endif
