#include "mysql_sentry.h"
#include <mysql/mysql.h>
#include <xalloc.h>
#include <stdbool.h>
#include "cmd.h"
#include "init_queries.sql.h"

struct sentry
{
  MYSQL db_handle;
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
sentry_initialize (const struct mysql_options *opts)
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

  struct sentry *new = xmalloc (sizeof *new);

  new->db_handle = db_handle;
  return new;

 tables:
  mysql_close(&db_handle);
 connect:
  return NULL;
}
