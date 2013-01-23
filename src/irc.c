#include "irc.h"
#include <libircclient/libircclient.h>
#include <libircclient/libirc_rfcnumeric.h>
#include <stdlib.h>
#include <string.h>
#include "defaults.h"
#include "error.h"
#include "utility.h"
#include "sentry.h"

#define EVENT_CALLBACK(function_name) void                              \
  function_name (irc_session_t * session, const char *event,            \
                 const char *origin, const char **params, unsigned int count)

struct context {
  SENTRY *sentry;
  const struct irc_options *opts;
};

EVENT_CALLBACK(event_nick)
{

}


static
EVENT_CALLBACK (event_join)
{
  const char *nickname =  origin;
  const char *channel = *params;
  const struct context *ctx = irc_get_ctx(session);

  printf ("%s joined %s\n", nickname, channel);
  fflush(stdout);
  sentry_channel_presence_clear(ctx, channel);
  irc_cmd_names(session, channel);
}

static
EVENT_CALLBACK (event_channel)
{

}

static inline void
event_numeric_namreply(struct context *context, const char *channel,
                       const char *nicknames)
{
  char *nicknames_buf = strdup(nicknames);

  for (const char *nickname = strtok(nicknames_buf, " ");
       nickname;
       nickname = strtok(NULL, " "))
    {
      sentry_channel_presence_add(context->sentry, channel, nickname);
    }

  free(nicknames_buf);
}

static void
event_numeric (irc_session_t * session, unsigned int
	       event, const char *origin, const char **params,
	       unsigned int count)
{
  struct context *context = irc_get_ctx(session);

  switch (event) {
  case LIBIRC_RFC_RPL_NAMREPLY:
    assert(count >= 4);
    const char *channel = params[2];
    const char *nicknames = params[3];
    event_numeric_namreply(context, channel, nicknames);
    break;
  }
}

static
EVENT_CALLBACK (event_connect)
{
  const struct context *ctx =  irc_get_ctx (session);
  printf("Connected!");
  for (char **chan = ctx->opts->channels; *chan; ++chan) {
    printf("sending connect request for %s\n", *chan);
    irc_cmd_join (session, *chan, NULL);
  }
  fflush(stdout);
}

/* Question to libircclient. Why not const? */
static irc_callbacks_t callbacks = {
  .event_channel = event_channel,
  .event_join = event_join,
  .event_connect = event_connect,
  .event_numeric = event_numeric,
  .event_nick =  event_nick
};

bool
start_listen_irc (const struct irc_options *opts,
		  SENTRY *sentry)
{
  irc_session_t *session = irc_create_session (&callbacks);
  struct context context = {
    .sentry = sentry,
    .opts = opts
  };

  if (!session)
    fatal ("Failed to create session");
  irc_set_ctx (session, &context);
  int error = irc_connect (session,
                           opts->server ? opts->server : default_server,
                           opts->port ? opts->port : default_port, NULL,	/* Password*/
			   opts->nick ? opts->nick : default_nick, NULL,
			   NULL);
  if (error)
    fatal ("error = %d, %s", error, irc_strerror (irc_errno (session)));
  error = irc_run (session);
  if (error)
    fatal ("error = %d, %s", error, irc_strerror (irc_errno (session)));
  irc_destroy_session (session);
  return true;
}
