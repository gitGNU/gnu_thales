<?php

//
// return the status of an user
//
// 0 = offline
// 1 = online
// 2 = away
//
// (c) tools.memphisnet.org 2003-2004
//

require_once('../common.php');

$nick = mysql_escape_string($_GET['n']);

$sql  = 'SELECT away FROM user WHERE nick = \'' . $nick . '\'';

$tmp = mysql_query($sql, $lid);

if (!mysql_num_rows($tmp))
{

	$status = 'offline';

} else {

	$tmp = mysql_fetch_array($tmp, MYSQL_ASSOC);

	if ('Y' == $tmp['away'])
	{

		$status = 'away';

	} else {

		$status = 'online';

	}

}

//mysql_close($db);

echo ($_GET['m'] == 'js') ? "document.write('$status')" : $status;

?>
