<?php

//
// count the number of servers on the network
// (c) tools.memphisnet.org 2003-2004
//

require_once('../common.php');

$sql  = 'SELECT COUNT(*) as servers_tc FROM server';

$tmp = mysql_query($sql, $lid);
$tmp = mysql_fetch_array($tmp, MYSQL_ASSOC);

mysql_close($lid);

echo ($_GET['m'] == 'js') ? "document.write('${tmp['servers_tc']}')" : $tmp['servers_tc'];

?>
