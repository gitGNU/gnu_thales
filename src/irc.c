#include "irc.h"
#include <stdlib.h>
#include <libircclient/libircclient.h>
#include <string.h>
#include "defaults.h"
#include "error.h"
#include "utility.h"

#define EVENT_CALLBACK(function_name) void                              \
  function_name (irc_session_t * session, const char *event,            \
                 const char *origin, const char **params, unsigned int count)

struct context {
  SENTRY *sentry;
  const struct irc_options *opts;
};

static bool
parse_message (const char *msg, char **name, char **payload)
{
  const char *const shebang = "#!";
  const char *payload_begin;

  *name = *payload = NULL;
  if (strcmp (msg, shebang))
    return false;		/* Is not command */

  /* Looks like command, but no terminating colon */
  if (!(payload_begin = strchr (msg, ':')))
    return false;

  *name = xrangedup (msg + strlen (shebang), payload_begin);
  *payload = xstrdup (payload_begin + 1);
  return true;
}

static
EVENT_CALLBACK (event_join)
{
  printf ("%s joined %s\n", origin, *params);
}

static
EVENT_CALLBACK (event_channel)
{

}

static void
event_numeric (irc_session_t * session, unsigned int
	       event, const char *origin, const char **params,
	       unsigned int count)
{
  printf ("Event =%d", event);
}


static
EVENT_CALLBACK (event_connect)
{
  const struct context *ctx =  irc_get_ctx (session);
  for (char **chan = ctx->opts->channels; *chan; ++chan)
    irc_cmd_join (session, *chan, NULL);
}

/* Question to libircclient. Why not const? */
static irc_callbacks_t callbacks = {
  .event_channel = event_channel,
  .event_join = event_join,
  .event_connect = event_connect,
  .event_numeric = event_numeric
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
                           opts->port ? opts->port : default_port, NULL,	/* Password. Not now. */
			   opts->nick ? opts->nick : default_nick, NULL,
			   NULL);
  if (error)
    fatal ("%s", irc_strerror (irc_errno (session)));
  error = irc_run (session);
  if (error)
    fatal ("error = %d, %s", error, irc_strerror (irc_errno (session)));
  irc_destroy_session (session);
  return true;
}
