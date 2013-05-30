(define-module (thales init-database)
  #:version (0 1 0)
  #:use-module (dbi dbi)
  #:export (initialize-database-tables))

(define +channel-init-query+ "CREATE TABLE IF NOT EXISTS channel (
  chanid int unsigned NOT NULL auto_increment,
  channel varchar(33) binary NOT NULL default '',
  topic text,
  topicauthor varchar(31) default NULL,
  topictime datetime default NULL,
  mode_lc enum('Y','N') NOT NULL default 'N',
  mode_lf enum('Y','N') NOT NULL default 'N',
  mode_li enum('Y','N') NOT NULL default 'N',
  mode_lj enum('Y','N') NOT NULL default 'N',
  mode_lk enum('Y','N') NOT NULL default 'N',
  mode_ll enum('Y','N') NOT NULL default 'N',
  mode_lm enum('Y','N') NOT NULL default 'N',
  mode_ln enum('Y','N') NOT NULL default 'N',
  mode_lp enum('Y','N') NOT NULL default 'N',
  mode_lr enum('Y','N') NOT NULL default 'N',
  mode_ls enum('Y','N') NOT NULL default 'N',
  mode_lt enum('Y','N') NOT NULL default 'N',
  mode_lu enum('Y','N') NOT NULL default 'N',
  mode_ly enum('Y','N') NOT NULL default 'N',
  mode_lz enum('Y','N') NOT NULL default 'N',
  mode_ua enum('Y','N') NOT NULL default 'N',
  mode_uc enum('Y','N') NOT NULL default 'N',
  mode_ud enum('Y','N') NOT NULL default 'N',
  mode_ug enum('Y','N') NOT NULL default 'N',
  mode_uh enum('Y','N') NOT NULL default 'N',
  mode_uk enum('Y','N') NOT NULL default 'N',
  mode_ul enum('Y','N') NOT NULL default 'N',
  mode_um enum('Y','N') NOT NULL default 'N',
  mode_un enum('Y','N') NOT NULL default 'N',
  mode_uo enum('Y','N') NOT NULL default 'N',
  mode_uq enum('Y','N') NOT NULL default 'N',
  mode_ur enum('Y','N') NOT NULL default 'N',
  mode_us enum('Y','N') NOT NULL default 'N',
  mode_ut enum('Y','N') NOT NULL default 'N',
  mode_uv enum('Y','N') NOT NULL default 'N',
  mode_ux enum('Y','N') NOT NULL default 'N',
  mode_uu enum('Y','N') NOT NULL default 'N',
  mode_lf_data varchar(20) NOT NULL default '',
  mode_lj_data varchar(5) NOT NULL default '',
  mode_lk_data varchar(23) NOT NULL default '',
  mode_ll_data int(10) NOT NULL default '0',
  mode_ul_data varchar(33) NOT NULL default '',
  PRIMARY KEY  (chanid),
  UNIQUE KEY channel (channel))")

(define +ison-init-query+ "CREATE TABLE IF NOT EXISTS ison (
  nickid int unsigned NOT NULL default '0',
  chanid int unsigned NOT NULL default '0',
  mode_la enum('Y','N') NOT NULL default 'N',
  mode_lh enum('Y','N') NOT NULL default 'N',
  mode_lo enum('Y','N') NOT NULL default 'N',
  mode_lq enum('Y','N') NOT NULL default 'N',
  mode_lv enum('Y','N') NOT NULL default 'N',
  PRIMARY KEY  (nickid,chanid),
  KEY nickid (nickid),
  KEY chanid (chanid)
);")

(define (initialize-database-tables db-obj)
  (map (lambda (query) (dbi-query db-obj (eval query (resolve-module '(thales init-database))))
               (let* ((status (dbi-get_status db-obj))
                      (err-code (car status)) (err-message (cdr status)))
                 (unless (zero? err-code)
                   (error (format #f
                                  "Error while evaluating ~a database query.\nError message: ~a."
                                  query err-message)))))
       '(+channel-init-query+ +ison-init-query+)))
