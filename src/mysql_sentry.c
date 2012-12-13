#include "mysql_sentry.h"
#include <mysql/mysql.h>
#include <xalloc.h>
#include "cmd.h"

typedef struct sentry
{
  MYSQL db_handle;
} SENTRY;

SENTRY *
sentry_initialize (const struct mysql_options *opts)
{
  MYSQL db_handle;
  mysql_init (&db_handle);
  if (!mysql_real_connect (&db_handle, opts->host, opts->user,
			   opts->password, opts->database, opts->port,
			   NULL, 0))
    {
      fprintf(stderr, "Failed to connect to database: Error: %s\n",
          mysql_error(&mysql));
      return NULL;
    }
  struct sentry *new = xmalloc (sizeof *new);
  new->db_handle = db_handle;
  return new;
}
