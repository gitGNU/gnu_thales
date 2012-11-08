#include "irc.h"
#include <stdlib.h>
#include <libircclient/libircclient.h>
#include <string.h>
#include "defaults.h"
#include "error.h"
#include "utility.h"

static bool
parse_message (const char *msg, char **name, char **payload)
{
  const char *const shebang = "#!";
  const char *payload_begin;

  *name = *payload = NULL;
  if (strncmp (msg, shebang, strlen (shebang)))
    return false;		/* Is not command */

  /* Looks like command, but no terminating colon */
  if (!(payload_begin = strchr (msg, ':')))
    return false;

  *name = xrangedup (msg + strlen (shebang), payload_begin);
  *payload = xstrdup (payload_begin + 1);
  return true;
}

static void
on_message (irc_session_t * session, const char *event,
	    const char *origin, const char **params, unsigned int count)
{

}

/* Question to libircclient. Why not const? */
static irc_callbacks_t callbacks = {
  .event_channel = on_message
};

bool
start_listen_irc (const struct cmd_options *opts,
		  const struct list_head *workers)
{
  irc_session_t *session;
  int error;
  session = irc_create_session (&callbacks);
  if (!session)
    fatal ("Failed to create session");
  /* Explicit cast is valid */
  irc_set_ctx (session, (void *) workers);
  error = irc_connect(session,
                      opts->server ? opts->server : default_server,
                      opts->port ? opts -> port : default_port,
                      NULL, /* Password. Not now. */
                      opts->nick ? opts->nick : default_nick,
                      NULL, NULL);
  if (error)
    fatal("Failed to connect: %s", irc_strerror(irc_errno(session)));

  for (const char **chan = opts->channels; *chan; ++chan)
    irc_cmd_join(session, *chan, NULL);

  irc_run(session);
  irc_destroy_session(session);
  return true;
}
