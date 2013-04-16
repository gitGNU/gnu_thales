#include <guile/2.0/libguile.h>
#include <libguile.h>
#include <libircclient/libircclient.h>
#include <assert.h>
#include <ctype.h>
static scm_t_bits irc_session_tag;
static irc_callbacks_t scm_irc_callbacks;
struct scm_t_irc_session {
  irc_session_t *session;
  SCM callback_alist;
  SCM self;
  SCM closure; /* third level of indirection */
};

/* Robust only for ascii string. */
static inline void
string_tolower(char *str)
{
  while (*str) {
    *str = (char) tolower(*str);
    str++;
  }
}

static inline char*
guarded_scm_to_string(SCM string)
{
  if (SCM_UNBNDP(string))
    return NULL;

  char *str = scm_to_locale_string(string);
  scm_dynwind_free(str);
  return str;
}

static inline SCM
scm_from_string_array(const char **args, unsigned int count)
{
   SCM result = SCM_EOL;
   for (unsigned int index = 0; index != count; ++index)
     {
      SCM current_parameter = scm_from_locale_string(args[index]);
      SCM current_parameter_list = scm_list_1(current_parameter);

      result = scm_append(scm_list_2(current_parameter_list, result));
    }
   return result;
}

static void
generic_event_handler(irc_session_t *sesion, const char *event,
       const char *origin, const char **params, unsigned int count)
{
  enum { symbol_buffer_size = 512 };
  char symbol_buffer[symbol_buffer_size];
  struct scm_t_irc_session *scm_irc_session = irc_get_ctx(sesion);
  SCM scm_params = scm_from_string_array(params, count);

  snprintf(symbol_buffer, symbol_buffer_size, "%s_event", event);
  string_tolower(symbol_buffer);
  SCM event_symbol = scm_from_locale_symbol(symbol_buffer);
  SCM func = scm_assq_ref(scm_irc_session->callback_alist, event_symbol);
  scm_call_3(func, scm_irc_session->self, scm_from_locale_string(origin), scm_params);
}

SCM_DEFINE(scm_create_irc_session,
           "create-irc-session", 1, 1, 0,
           (SCM callback_alist, SCM closure),
           "Create irc session with callback functions specified by alist. This is"
           "low-level api you should not use directly in user-level scheme code.")
{
  SCM smob;
  struct scm_t_irc_session *scm_irc_session;
  irc_session_t *irc_session = irc_create_session(&scm_irc_callbacks);
  /* check for irc_session != NULL */
  scm_irc_session = scm_gc_malloc(sizeof(struct scm_t_irc_session), "irc-session");
  irc_set_ctx(irc_session, scm_irc_session);

  scm_irc_session->callback_alist = callback_alist;
  scm_irc_session->closure = closure;
  scm_irc_session->session = irc_session;

  smob = scm_new_smob(irc_session_tag,  (scm_t_bits) scm_irc_session);
  scm_irc_session->self = smob;

  return smob;
}

SCM_DEFINE(scm_irc_connect, "irc-connect", 4, 3, 0,
           (SCM scm_session, SCM scm_server, SCM scm_port, SCM scm_nick,
            SCM scm_server_pass, SCM scm_username, SCM scm_realname),
           "This function prepares and initiates a connection to the IRC"
           "server. The connection is done asynchronously (see"
           "event_connect), so the success return value means that"
           "connection was initiated successfully.")
{
  scm_assert_smob_type(irc_session_tag, scm_session);
  struct scm_irc_session *c_scm_session = SCM_SMOB_DATA(scm_session);

  scm_dynwind_begin(0);
  char *server = guarded_scm_to_string(scm_server);
  unsigned short int port = scm_to_uint16(scm_port);
  char *nick = scm_to_ascii_string(scm_nick);
  char *server_pass = scm_to_ascii_string(scm_server_pass);
  char *username = scm_to_ascii_string(scm_username);
  char *realname = scm_to_ascii_string(scm_realname);

}

void
init_message_irc()
{
  irc_session_tag=scm_make_smob_type("irc-session", sizeof(struct scm_t_irc_session));
  scm_irc_callbacks.event_connect = generic_event_handler;
#ifndef SCM_MAGIC_SNARFER
#include "message_irc.x"
#endif
}
