#include "sentry.h"
#include <xalloc.h>
#include <stdbool.h>
#include <dbi/dbi.h>
#include "cmd.h"
#include "init_queries.sql.h"

struct sentry
{
char *unused;
};

static inline bool
initialize_tables(MYSQL *db_handle)
{
  for (const char *query = *init_queries(); *query; ++query)
    if (!mysql_query(db_handle, query))
      return false;
  return true;
}

SENTRY *
sentry_initialize (const struct mysql_options *opts, const char *server)
{
  MYSQL db_handle;
  mysql_init (&db_handle);
  if (!mysql_real_connect (&db_handle, opts->host, opts->username,
			   opts->password, opts->database, opts->port,
			   NULL, 0))
    {
      fprintf(stderr, "Failed to connect to database: Error: %s\n",
              mysql_error(&db_handle));
      goto connect;
  }
  if (!initialize_tables(&db_handle)) {
    fprintf(stderr, "Failed to connect to database: Error: %s\n",
            mysql_error(&db_handle));
    goto tables;
  }

  struct sentry *new = xcalloc (1, sizeof *new);

  new->db_handle = db_handle;
  new->server = xstrdup(server);
  return new;

 tables:
  mysql_close(&db_handle);
 connect:
  return NULL;
}

void
sentry_channel_presence_clear(SENTRY *sentry, const char *channel)
{
  static const char *query =
}

void
sentry_channel_presence_add(SENTRY *sentry, const char *channel,
                            const char *nickname)
{
  const char *query = "INSERT INTO presence VALUES (nickid, chanid, servid) where

}
