<?php

//
// get the current topic of a chan
// (c) tools.memphisnet.org 2003-2004
//

require_once('../common.php');

if (ereg('^#', $_GET['c'])) { $chan = $_GET['c']; } else { $chan = '#' . $_GET['c']; }
$chan = mysql_escape_string($chan);
$chan = strtolower($chan);

$sql  = 'SELECT topic FROM chan WHERE ';
$sql .= 'channel = \'' . $chan . '\'';

$tmp = mysql_query($sql, $lid);
$tmp = mysql_fetch_array($tmp, MYSQL_ASSOC);

mysql_close($lid);

echo ($_GET['m'] == 'js') ? "document.write('". addslashes($tmp['topic']) . "')" : $tmp['topic'];

?>
