#include "irc.h"
#include <stdlib.h>
#include <libircclient/libircclient.h>
#include <libircclient/libirc_rfcnumeric.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <signal.h>
#include <cmd.h>
#include "error.h"
#include "writer.h"

static volatile sig_atomic_t exit_flag;


static inline void
check_disconnect (irc_session_t * session)
{
  if (exit_flag)
    irc_disconnect (session);
}

static inline const char *
purge_nick (const char *nick)
{
  enum
  { nickname_max_length = 256, max_simultaneous_calls = 8 };
  static char buffer[max_simultaneous_calls][nickname_max_length];
  static int call_index = 0;
  call_index = (call_index + 1) % max_simultaneous_calls;
  irc_target_get_nick (nick, buffer[call_index], nickname_max_length);
  return buffer[call_index];
}

#define EVENT_CALLBACK(function_name)  void                             \
  function_name (irc_session_t * session, const char *event,            \
                 const char *origin, const char **params, unsigned int count)

EVENT_CALLBACK (event_part)
{
  const struct work_options *context = irc_get_ctx (session);
  const char *nickname = purge_nick (origin);
  const char *channel = *params;
  const char *reason = params[1];

  writer_channel_presence_remove (context, channel, nickname);
  check_disconnect (session);
}

EVENT_CALLBACK (event_topic)
{
  const struct work_options *context = irc_get_ctx (session);
  const char *nickname = purge_nick (origin);
  const char *channel = *params;
  const char *new_topic = params[1];

  writer_channel_topic_update (context, channel, nickname, new_topic);
}

EVENT_CALLBACK (event_kick)
{
  const struct work_options *context = irc_get_ctx (session);
  const char *kicker = purge_nick (origin);
  const char *channel = *params;
  const char *victim = purge_nick (params[1]);
  const char *reason = params[2];

  writer_channel_presence_remove (context, channel, victim);
}

EVENT_CALLBACK (event_nick)
{
  const struct work_options *context = irc_get_ctx (session);
  const char *oldnick = purge_nick (origin);
  const char *newnick = purge_nick (*params);


  writer_nick_change (context, oldnick, newnick);
}

static
EVENT_CALLBACK (event_quit)
{
  const struct work_options *context = irc_get_ctx (session);
  const char *nickname = purge_nick (origin);

  writer_nick_quit (context, nickname);
}


static
EVENT_CALLBACK (event_join)
{
  const struct work_options *context = irc_get_ctx (session);
  const char *nickname = purge_nick (origin);
  const char *channel = *params;
  const char *own_nickname = context->nickname;

  if (!strcmp (own_nickname, nickname))
    writer_channel_presence_clear (context, channel);
  else
    writer_channel_presence_add (context, channel, nickname);
}

static
EVENT_CALLBACK (event_channel)
{
  const struct work_options *context = irc_get_ctx (session);
  const char *nickname = purge_nick (origin);
  const char *channel = *params;
  const char *text = params[1];

  debug ("%s", text);
}

static inline void
event_numeric_namreply (irc_session_t * session, const char *channel,
			const char *nicknames)
{
  struct work_options *context = irc_get_ctx (session);
  char *nicknames_buf = strdup (nicknames);

  for (const char *nickname = strtok (nicknames_buf, " ");
       nickname; nickname = strtok (NULL, " "))
    {
      writer_channel_presence_add (context, channel, purge_nick (nickname));
      irc_cmd_whois (session, nickname);
    }

  free (nicknames_buf);
}

static inline void
event_numeric_whoisuser (const char *nickname,
			 const char *hostname, const char *server)
{

}

#define NUMERIC_OUTPUT_ARGS() \
  fprintf(stderr, "*** Origin: %s ***\n", origin);                   \
  fprintf(stderr, "*** Parameters: ***\n");         \
  for (int ne_index = 0; ne_index != count;  ne_index++) \
    fprintf(stderr, "%s\n", params[ne_index]);            \
  fprintf(stderr, "*********\n");

static void
event_numeric (irc_session_t * session, unsigned int
	       event, const char *origin, const char **params,
	       unsigned int count)
{
  switch (event)
    {
    case LIBIRC_RFC_RPL_NAMREPLY:
      assert (count >= 4);
      const char *channel = params[2];
      const char *nicknames = params[3];
      event_numeric_namreply (session, channel, nicknames);
      break;
    case LIBIRC_RFC_RPL_WHOISUSER:
      assert (count == 6);
      const char *server = origin;
      const char *nickname = params[1];
      const char *hostname = params[3];
      const char *realname = params[5];
      event_numeric_whoisuser (nickname, hostname, server);
      NUMERIC_OUTPUT_ARGS ();
      break;
    }
}

static
EVENT_CALLBACK (event_connect)
{
  const struct work_options *context = irc_get_ctx (session);
  debug ("Connected!");
  for (char **chan = context->channels; *chan; ++chan)
    irc_cmd_join (session, *chan, NULL);
}

/* Question to libircclient. Why not const? */
static irc_callbacks_t callbacks = {
  .event_channel = event_channel,
  .event_join = event_join,
  .event_connect = event_connect,
  .event_numeric = event_numeric,
  .event_nick = event_nick
};

#define check_irc_error(session, err) if (err) {fatal ("error = %d, %s", err, irc_strerror (irc_errno (session))); }
bool
client_start_listen (const struct work_options *opts)
{
  irc_session_t *session = irc_create_session (&callbacks);

  if (!session)
    fatal ("Failed to create session");

  irc_set_ctx (session, opts);

  int err = irc_connect (session,
			 opts->server, opts->port, NULL,	/* Password */
			 opts->nickname, NULL, NULL);
  check_irc_error (session, err);

  err = irc_run (session);
  check_irc_error (session, err);

  irc_destroy_session (session);
  return true;
}

#undef check_irc_error

void
client_stop_listen (void)
{
  exit_flag = 1;
}
