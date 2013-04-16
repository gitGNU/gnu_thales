#include "definitions.h"
#include <assert.h>
#include <ctype.h>
static irc_callbacks_t irc_callbacks;
static SCM irc_error_symbol;
static scm_t_bits scm_t_irc_session_tag;

void
scm_throw_from_irc_session (irc_session_t * session)
{
  int errno = irc_errno (session);
  const char *errmsg = irc_strerror (errno);
  SCM scm_errmsg = scm_from_locale_string (errmsg);
  scm_throw (irc_error_symbol, scm_list_1 (scm_errmsg));
}

irc_session_t*
scm_to_irc_session(SCM scm_session)
{
  scm_assert_smob_type (scm_t_irc_session_tag, scm_session);
  struct scm_t_irc_session *scm_t_session = SCM_SMOB_DATA(scm_session);
  assert(scm_t_session);
  return scm_t_session->session;
}

char*
guarded_scm_to_ascii_string (SCM string_or_false)
{
  if (scm_is_false (string_or_false))
    return NULL;

  char *str = scm_to_locale_string (string_or_false);
  scm_dynwind_free (str);
  return str;
}

static inline SCM
scm_from_string_array (const char **args, unsigned int count)
{
  SCM result = SCM_EOL;
  for (unsigned int index = 0; index != count; ++index)
    {
      SCM current_parameter = scm_from_locale_string (args[index]);
      SCM current_parameter_list = scm_list_1 (current_parameter);

      result = scm_append (scm_list_2 (current_parameter_list, result));
    }
  return scm_reverse(result);
}

static void
generic_event_handler (irc_session_t * sesion, const char *event,
		       const char *origin, const char **params,
		       unsigned int count)
{
  struct scm_t_irc_session *scm_irc_session = irc_get_ctx (sesion);
  SCM scm_params = scm_from_string_array (params, count);

  scm_call_4 (scm_irc_session->event_dispatcher, scm_irc_session->closure,
	      scm_from_locale_string (event), scm_from_locale_string (origin),
	      scm_params);
}

SCM_DEFINE (scm_create_irc_session,
	    "irc-create-session", 3, 0, 0,
	    (SCM closure, SCM event_dispatcher, SCM numeric_dispatcher),
	    "Create irc session with callback functions specified by alist. This is"
	    "low-level api you should not use directly in user-level scheme code.")
{
  struct scm_t_irc_session *scm_irc_session;
  irc_session_t *irc_session = irc_create_session (&irc_callbacks);
  scm_irc_session =
    scm_gc_malloc (sizeof (struct scm_t_irc_session), "irc-session");
  irc_set_ctx (irc_session, scm_irc_session);

  scm_irc_session->closure = closure;
  scm_irc_session->event_dispatcher = event_dispatcher;
  scm_irc_session->numeric_dispatcher = numeric_dispatcher;
  scm_irc_session->session = irc_session;

  return scm_new_smob (scm_t_irc_session_tag, (scm_t_bits) scm_irc_session);
}

SCM_DEFINE (scm_irc_connect, "irc-connect", 8, 0, 0,
	    (SCM scm_session, SCM scm_server, SCM scm_port,
	     SCM scm_nick, SCM scm_server_pass, SCM scm_username,
	     SCM scm_realname, SCM scm_use_ipv6_p),
	    "This function prepares and initiates a connection to the IRC"
	    "server. The connection is done asynchronously (see"
	    "event_connect), so the success return value means that"
	    "connection was initiated successfully.")
{
  irc_session_t *session = scm_to_irc_session(scm_session);
  scm_dynwind_begin (0);
  char *server = guarded_scm_to_ascii_string (scm_server);
  char *nick = guarded_scm_to_ascii_string (scm_nick);
  char *server_pass = guarded_scm_to_ascii_string (scm_server_pass);
  char *username = guarded_scm_to_ascii_string (scm_username);
  char *realname = guarded_scm_to_ascii_string (scm_realname);
  unsigned short int port = scm_to_uint16 (scm_port);
  int (*connect_fn) (irc_session_t *, const char *, unsigned short,
		     const char *, const char *, const char *, const char *)
    = scm_to_bool (scm_use_ipv6_p) ? irc_connect6 : irc_connect;
  int error =
    (*connect_fn) (session, server, port, server_pass,
		   nick, username, realname);
  if (error)
    scm_throw_from_irc_session (session);

  scm_dynwind_end ();
  scm_remember_upto_here_1 (scm_session);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE (scm_irc_run, "irc-run", 1, 0, 0,
	    (SCM scm_session),
	    "This function goes into the loop, processing the IRC events, and"
	    "calling appropriate callbacks. This function will not return until"
	    "the server connection is terminated – either by server, or by calling"
	    "irc_cmd_quit. This function should be used, if you don’t need"
	    "asynchronous request processing (i.e. your bot just reacts on the"
	    "events, and doesn’t generate it asynchronously). Even in last case,"
	    "you still can call irc_run, and start the asynchronous thread in"
	    "event_connect handler.")
{
  scm_assert_smob_type (scm_t_irc_session_tag, scm_session);
  struct scm_t_irc_session *c_scm_session = SCM_SMOB_DATA (scm_session);
  irc_run (c_scm_session->session);
  /*
   * Error is not checked due bug in underlying library.
   */
  scm_remember_upto_here_1 (scm_session);
  return SCM_UNSPECIFIED;
}

static SCM
scm_mark_irc_session (SCM s)
{
  struct scm_t_irc_session *scm_session = SCM_SMOB_DATA(s);

  scm_gc_mark (scm_session->event_dispatcher);
  scm_gc_mark (scm_session->numeric_dispatcher);
  return scm_session->closure;
}

static size_t
scm_free_irc_session (SCM s)
{
  struct scm_t_irc_session *scm_session = SCM_SMOB_DATA(s);
  if (scm_session->session)
    {
      irc_disconnect(scm_session->session);
      irc_destroy_session (scm_session->session);
    }
  scm_session->session = NULL;
  return 0;
}

SCM_DEFINE (scm_irc_disconnect, "irc-disconnect", 1, 0, 0,
	    (SCM s), "Rude disconnect.")
{
  irc_session_t *session = scm_to_irc_session(s);
  irc_disconnect(session);
  return SCM_UNSPECIFIED;
}


static inline void
setup_generic_event_handler (irc_callbacks_t * cb)
{
  cb->event_connect
    = cb->event_nick
    = cb->event_quit
    = cb->event_join
    = cb->event_part
    = cb->event_mode
    = cb->event_umode
    = cb->event_topic
    = cb->event_kick
    = cb->event_channel
    = cb->event_privmsg
    = cb->event_notice
    = cb->event_channel_notice
    = cb->event_invite = generic_event_handler;
}

#include "cmd.c"
#include "misc.c"
void
scm_init_im_irc ()
{
  irc_error_symbol = scm_from_locale_symbol ("irc-error");
  setup_generic_event_handler (&irc_callbacks);
  scm_t_irc_session_tag =
    scm_make_smob_type ("irc-session", sizeof (struct scm_t_irc_session));
  scm_set_smob_mark (scm_t_irc_session_tag, scm_mark_irc_session);
  scm_set_smob_free (scm_t_irc_session_tag, scm_free_irc_session);

#ifndef SCM_MAGIC_SNARFER
#include "init.x"
#endif
}
