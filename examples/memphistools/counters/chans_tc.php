<?php

//
// count the number of chans on the network
// (c) tools.memphisnet.org 2003-2004
//

require_once('../common.php');

$sql  = 'SELECT COUNT(*) as chans_tc FROM chan';

$tmp = mysql_query($sql, $lid);
$tmp = mysql_fetch_array($tmp, MYSQL_ASSOC);

mysql_close($lid);

echo ($_GET['m'] == 'js') ? "document.write('${tmp['chans_tc']}')" : $tmp['chans_tc'];

?>