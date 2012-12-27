#ifndef INIT_QUERIES_H
#define INIT_QUERIES_H
static const char *channel_tbl =
  "CREATE TABLE IF NOT EXISTS channel ("
  "chanid int unsigned NOT NULL auto_increment,"
  "channel varchar(33) binary NOT NULL default '',"
  "topic text,"
  "topicauthor varchar(31) default NULL,"
  "topictime datetime default NULL,"
  "PRIMARY KEY  (chanid),"
  "UNIQUE KEY channel (channel))";

static const char *user_tbl =
  "CREATE TABLE IF NOT EXISTS user ("
  "nickid int unsigned NOT NULL auto_increment,"
  "nick varchar(31) NOT NULL default '',"
  "realname varchar(51) NOT NULL default '',"
  "hostname varchar(64) NOT NULL default '',"
  "hiddenhostname varchar(64) NOT NULL default '',"
  "username varchar(11) NOT NULL default '',"
  "swhois varchar(32) NOT NULL default '',"
  "connecttime datetime NOT NULL default '0000-00-00 00:00:00',"
  "servid int unsigned NOT NULL default '0',"
  "away enum('Y','N') NOT NULL default 'N',"
  "awaymsg text,"
  "online enum('Y','N') NOT NULL DEFAULT 'Y',"
  "lastquit datetime default NULL,"
  "PRIMARY KEY  (nickid),"
  "UNIQUE KEY nick (nick),"
  "KEY servid (servid))";

static const char * server_tbl =
  "CREATE TABLE IF NOT EXISTS server ("
  "servid int unsigned NOT NULL auto_increment,"
  "server varchar(64) NOT NULL default '',"
  "connecttime datetime default NULL,"
  "PRIMARY KEY  (servid),"
  "UNIQUE KEY server (server))";

static const char *presence_tbl =
  "CREATE TABLE IF NOT EXISTS presence ("
  "nickid int unsigned NOT NULL default '0',"
  "chanid int unsigned NOT NULL default '0',"
  "servid int unsigned NOT NULL default '0',"
  "PRIMARY KEY  (nickid,chanid,servid),"
  "KEY nickid (nickid),"
  "KEY chanid (chanid))";

/* Stupid restriction of C */
static inline const char **const
init_queries ()
{
  enum { query_count = 5 };
  static const char *queries[query_count] = { 0 };
  queries[0] = user_tbl;
  queries[1] = presence_tbl;
  queries[2] = server_tbl;
  queries[3] = channel_tbl;
  queries[4] = NULL;
  return queries;
}






#endif
