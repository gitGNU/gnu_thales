<?php

//
// get the list of users on a channel
// (c) tools.memphisnet.org 2003-2004
//

require_once('../common.php');

$chan = mysql_escape_string($_GET['c']);
$chan = strtolower($chan);
//$m = mysql_escape_string($_GET['m']);

if ($_GET['m'] == 'js') { echo "document.write('"; }

$query='SELECT user.nick, COUNT(user.nick), ison.mode_lo, ison.mode_lh, ison.mode_lv, ison.mode_lq FROM ison, chan, user WHERE chan.chanid = ison.chanid AND ison.nickid = user.nickid AND chan.channel = \'\#' . $chan . '\' AND chan.mode_lp = \'N\' AND chan.mode_ls = \'N\' GROUP BY user.nick';
$result=mysql_query($query);
$cpt= mysql_num_rows($result);

$sql ='SELECT user.nick, ison.mode_lo, ison.mode_lh, ison.mode_lv, ison.mode_lq, ison.mode_la FROM ison, chan, user WHERE chan.chanid = ison.chanid AND ison.nickid = user.nickid AND chan.channel = \'\#' . $chan . '\' AND chan.mode_lp = \'N\' AND chan.mode_ls = \'N\' ORDER BY ison.mode_lq, ison.mode_la, ison.mode_lo, ison.mode_lh, ison.mode_lv';

$result = mysql_query($sql, $lid);
$num_of_rows = mysql_num_rows($result) or die ("Pas de correspondances");
while ($tmp = mysql_fetch_row($result))
{
	$ison = $tmp[0];
	$mo = $tmp[1];
	$mh = $tmp[2];
	$mv = $tmp[3];
	$mq = $tmp[4];
	$ma = $tmp[5];
	
	$mode="";
	if ($mv == "Y") { $mode="+"; }
	if ($mh == "Y") { $mode="%"; }
	if ($mo == "Y") { $mode="@"; }
	if ($ma == "Y") { $mode="&"; }
	if ($mq == "Y") { $mode="~"; }
	$cpt--;
	if ($cpt == "0") { $sep=""; }
	else { $sep=", "; }
	echo $mode . $ison . $sep;
}

mysql_close($lid);

if ($_GET['m'] == 'js') { echo "')"; }
?>
