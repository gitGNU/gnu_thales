<?php

//
// return the status of a server
//
// 0 = offline
// 1 = online
//
// (c) tools.memphisnet.org 2003-2004
//

require_once('../common.php');

$server = mysql_escape_string($_GET['s']);
$sql    = "SELECT servid FROM server WHERE server = '$server';";

$tmp = mysql_query($sql, $lid);
$tmp = mysql_fetch_array($tmp, MYSQL_ASSOC);

mysql_close($lid);

echo $tmp ? 1 : 0;

?>
