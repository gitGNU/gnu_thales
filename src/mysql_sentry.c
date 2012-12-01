#include "mysql_sentry.h"
#include <mysql/mysql.h>
#include "sentry.h"
#include <xalloc.h>

struct statements {
  MYSQL_RES *last_activity;
};
struct mysql_sentry
{
  struct sentry sentry;
  MYSQL db_handle;
  struct statements statements;
};
static void mysql_sentry_join_event (struct sentry *, const char *user,
				     const char *channel);
static void mysql_sentry_message_event (const struct sentry *,
					const char *user, const char *channel,
					const char *msg);
static void mysql_sentry_part_event (struct sentry *, const char *user,
				     const char *channel);

static inline void
initialize_vtable (struct sentry *sentry)
{
  sentry->join_event = mysql_sentry_join_event;
  sentry->message_event = mysql_sentry_message_event;
  sentry->part_event = mysql_sentry_part_event;
}

static inline

struct sentry *
create_mysql_sentry (const struct envz *envz)
{
  struct mysql_sentry *new = xmalloc (sizeof *new);
  DECLARE_ENVZ_GET (envz, host);
  DECLARE_ENVZ_GET (envz, username);
  DECLARE_ENVZ_GET (envz, password);
  DECLARE_ENVZ_GET(envz, prefix);
  DECLARE_ENVZ_GET(envz, database);
  initialize_vtable (&new->sentry);

  if (!mysql_connect(&new->db_handle, host, username, password)) {
    warning("Failed to connect to mysql server with username %s", username);
    free(new);
    return NULL;
  }
  if (!database ||  mysql_select_db(&new->db_handle, database)) {
    warning("Failed to select database %s", database);
    free(new);
    return NULL;
  }


  //  mysql_connect(new->db_handle,
}
