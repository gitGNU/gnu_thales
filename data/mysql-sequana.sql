DROP TABLE IF EXISTS chan;
DROP TABLE IF EXISTS ison;
DROP TABLE IF EXISTS server;
DROP TABLE IF EXISTS user;
DROP TABLE IF EXISTS maxvalues;

CREATE TABLE chan (
  chanid int unsigned NOT NULL auto_increment,
  channel varchar(33) binary NOT NULL default '',
  topic text,
  topicauthor varchar(31) default NULL,
  topictime datetime default NULL,
  mode_lc enum('Y','N') NOT NULL default 'N',
  mode_li enum('Y','N') NOT NULL default 'N',
  mode_lk enum('Y','N') NOT NULL default 'N',
  mode_ll enum('Y','N') NOT NULL default 'N',
  mode_lm enum('Y','N') NOT NULL default 'N',
  mode_ln enum('Y','N') NOT NULL default 'N',
  mode_lp enum('Y','N') NOT NULL default 'N',
  mode_lr enum('Y','N') NOT NULL default 'N',      
  mode_ls enum('Y','N') NOT NULL default 'N',
  mode_lt enum('Y','N') NOT NULL default 'N',
  mode_ul enum('Y','N') NOT NULL default 'N',
  mode_um enum('Y','N') NOT NULL default 'N',
  mode_ur enum('Y','N') NOT NULL default 'N',
  mode_uo enum('Y','N') NOT NULL default 'N',
  mode_ll_data int(10) NOT NULL default '0',
  mode_lk_data varchar(23) NOT NULL default '',
  UNIQUE KEY channel (channel),
  PRIMARY KEY  (chanid)
) TYPE=MyISAM;


CREATE TABLE ison (
  nickid int unsigned NOT NULL default '0',
  chanid int unsigned NOT NULL default '0',
  mode_lo enum('Y','N') NOT NULL default 'N',
  mode_lv enum('Y','N') NOT NULL default 'N',
  PRIMARY KEY  (nickid,chanid)
) TYPE=MyISAM;

CREATE TABLE server (
  servid int unsigned NOT NULL auto_increment,
  server varchar(64) NOT NULL default '',
  comment varchar(255) NOT NULL default '',
  linkedto tinyint(3) unsigned default NULL,
  connecttime datetime default NULL,
  online enum('Y','N') NOT NULL DEFAULT 'Y',
  lastsplit datetime default NULL,
  PRIMARY KEY  (servid),
  UNIQUE KEY server (server),
  KEY linkedto (linkedto)
) TYPE=MyISAM;


CREATE TABLE user (
  nickid int unsigned NOT NULL auto_increment,
  nick varchar(31) NOT NULL default '',
  realname varchar(51) NOT NULL default '',
  hostname varchar(64) NOT NULL default '',
  hiddenhostname varchar(64) NOT NULL default '',
  ipaddr varchar(16) NOT NULL default '',
  username varchar(11) NOT NULL default '',
  connecttime datetime NOT NULL default '0000-00-00 00:00:00',
  servid int unsigned NOT NULL default '0',
  away enum('Y','N') NOT NULL default 'N',
  awaymsg text,
  online enum('Y','N') NOT NULL DEFAULT 'Y',
  lastquit datetime default NULL,
  mode_la enum('Y','N') NOT NULL default 'N',  
  mode_lb enum('Y','N') NOT NULL default 'N',  
  mode_lc enum('Y','N') NOT NULL default 'N',  
  mode_ld enum('Y','N') NOT NULL default 'N',  
  mode_le enum('Y','N') NOT NULL default 'N',  
  mode_lf enum('Y','N') NOT NULL default 'N',  
  mode_lg enum('Y','N') NOT NULL default 'N',  
  mode_lh enum('Y','N') NOT NULL default 'N',
  mode_li enum('Y','N') NOT NULL default 'N',
  mode_lk enum('Y','N') NOT NULL default 'N',
  mode_ln enum('Y','N') NOT NULL default 'N',
  mode_lo enum('Y','N') NOT NULL default 'N',
  mode_lr enum('Y','N') NOT NULL default 'N',
  mode_ls enum('Y','N') NOT NULL default 'N',
  mode_lw enum('Y','N') NOT NULL default 'N',
  mode_lx enum('Y','N') NOT NULL default 'N',
  mode_ly enum('Y','N') NOT NULL default 'N',
  mode_ua enum('Y','N') NOT NULL default 'N',
  mode_ur enum('Y','N') NOT NULL default 'N',
  UNIQUE KEY nick (nick),
  PRIMARY KEY  (nickid),
  KEY servid (servid)
) TYPE=MyISAM;



CREATE TABLE maxvalues (
  type varchar(10) NOT NULL default '',
  val int unsigned NOT NULL default '0',
  time datetime NOT NULL default '0000-00-00 00:00:00',
  PRIMARY KEY(type)
) TYPE=MyISAM;