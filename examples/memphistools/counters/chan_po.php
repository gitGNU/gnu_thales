<?php

//
// count the number of people on a given chan
// (c) tools.memphisnet.org 2003-2004
//

require_once('../common.php');

if (ereg('^#', $_GET['c'])) { $chan = $_GET['c']; } else { $chan = '#' . $_GET['c']; }
$chan = strtolower(mysql_escape_string($chan));

$sql  = 'SELECT COUNT(*) as chan_po FROM ison, chan WHERE ';
$sql .= 'chan.channel = \'' . $chan . '\' ';
$sql .= 'AND ison.chanid = chan.chanid';

$tmp = mysql_query($sql, $lid);
$tmp = mysql_fetch_array($tmp, MYSQL_ASSOC);

mysql_close($lid);

echo ('js' == $_GET['m']) ? "document.write('${tmp['chan_po']}')" : $tmp['chan_po'];

?>
