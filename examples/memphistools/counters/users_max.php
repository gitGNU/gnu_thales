<?php

//
// users maxvalue
// (c) tools.memphisnet.org 2003-2004
//

require_once('../common.php');

$sql  = 'SELECT val FROM maxvalues WHERE type = \'users\'';

$tmp = mysql_query($sql, $lid);
$tmp = mysql_fetch_array($tmp, MYSQL_ASSOC);

mysql_close($lid);

echo ($_GET['m'] == 'js') ? "document.write('${tmp['val']}')" : $tmp['val'];

?>
