<?
/* ============================================================================ 
# /whois emulation for www (powered by Thales)
# (note: this is just an example script, add/modify as you wish)
# 
#    File:	dowhois.php
#    Author:    Partizanu | partizanu@netchat.ro
#    Version:	0.1
#    Input:	mySQL data from Thales tables
#    Output:	whois info (a la mIRC)
#
#
#    Make sure to:
#	1) Change $mysqluser and $mysqlpassword
#	2) set permision and flush privileges in mysql so that script can connect
#	3) place the html and the php files in the same dir (or change paths in source)
#	4) IMPORTANT!!!: this script takes $nickname from the POST of the html file
#		This variable may be "dirty" (eg. bad javascript code, SQL inject etc)
#		Use whatever function makes you confortable to add security to this code (eg. strip_tags,addslashes,htmlspecialchars,quotemeta etc.)
#		The (main) reason why this script doesn't use em is because I (the author) don't want to get mail-bombed with "Your security sux, they hacked my site coz of your stupid script" so do your "security" thing :)
#	5) change isok variable from 1 to 0 so I can know that you read this :)
#	6) have fun
# ============================================================================*/

$mysqluser = "php";
$mysqlpassword = "phprulez";
$isok=1;



//===Don't change anything below this unless you know what you'r doing (yeah, right)

$link = mysql_connect ("localhost", $mysqluser, $mysqlpassword) or die ("Can't connect");
mysql_select_db("thales");



$q = mysql_query("select * from user where nick=\"$nickname\" ");
if (mysql_num_rows($q) < 1) 
	{
	mysql_close($link);
	die($nickname ." No such nick/channel");
	}

if ($isok==1) die("Read instructions from head of php file!");
while ($row = mysql_fetch_array($q))
	{
	$id = $row["nickid"];
	$rname = $row["realname"];
	$hostname = $row["hostname"];
	$username = $row["username"];
	$connecttime = $row["connecttime"];
	$servid  = $row["servid"];
	$away = $row["away"];
	if ($away == "Y") $awaymsg = $row["awaymsg"];
	$isircop = $row["mode_lo"];
	$issadmin = $row["mode_la"]; //bahamut only?
	$isreg = $row["mode_lr"]; //bahamut only? depends of type of services?
	}

$q_servers = mysql_query ("select server,comment from server where servid=$servid");
$r = mysql_fetch_row($q_servers);
$server = $r[0];
$scomm = $r[1];

$chid = Array();
$chname = Array();
$channels = NULL;
$q_channs = mysql_query ("select channel from ison,chan,user where nick = \"$nickname\" and (ison.chanid=chan.chanid and user.nickid=ison.nickid)");
while ($r = mysql_fetch_array ($q_channs))
       	{
       	$tmp = $r["channel"];
       	$q_mode = mysql_query("select mode_ls,mode_lp,chanid from chan where channel=\"$tmp\" ");
	$q_mode_r = mysql_fetch_row($q_mode);
       	if ($q_mode_r[0] == "N" && $q_mode_r[1] == "N") 
       		{
		$q_getop=mysql_query("select mode_lo,mode_lv from ison where chanid=$q_mode_r[2] and nickid=$id");
		$chmode = mysql_fetch_row($q_getop);
		if ($chmode[0]=="Y") $channels = $channels . " @" . $tmp;
       		elseif ($chmode[1]=="Y") $channels = $channels . " +" . $tmp;
       		else $channels = $channels . " " . $tmp;
       		}
       	}
mysql_close($link);


echo "<table width=\"550\" border=\"0\" cellspacing=\"0\" cellpadding=\"0\" align=\"center\">";
echo "<tr><td><font face=\"Arial, Helvetica, sans-serif\" size=\"2\">$nickname is ".$username."@".$hostname." * ".$rname."</font></td></tr>";
echo "<tr><td><font face=\"Arial, Helvetica, sans-serif\" size=\"2\">$nickname is connected since $connecttime</font></td></tr>";

if (!is_null($channels)) echo "<tr><td><font face=\"Arial, Helvetica, sans-serif\" size=\"2\">$nickname on $channels</font></td></tr>";
echo "<tr><td><font face=\"Arial, Helvetica, sans-serif\" size=\"2\">$nickname using $server $scomm</font></td></tr>";
if ($isreg == "Y") echo "<tr><td><font face=\"Arial, Helvetica, sans-serif\" size=\"2\">$nickname has identified for this nick</font></td></tr>";
if ($away == "N") echo "<tr><td><font face=\"Arial, Helvetica, sans-serif\" size=\"2\">$nickname is not away</font></td></tr>";
else echo "<tr><td><font face=\"Arial, Helvetica, sans-serif\" size=\"2\">$nickname is away: $awaymsg</font></td></tr>";
if ($isircop == "Y") echo "<tr><td><font face=\"Arial, Helvetica, sans-serif\" size=\"2\">$nickname is an IRC Operator</font></td></tr>";
if ($issadmin == "Y") echo "<tr><td><font face=\"Arial, Helvetica, sans-serif\" size=\"2\">$nickname is a Service Administrator</font></td></tr>";
echo "<tr><td align=\"center\"><font face=\"Arial, Helvetica, sans-serif\" size=\"2\"><hr></font></td></tr>";
echo "<tr><td align=\"center\"><font face=\"Arial, Helvetica, sans-serif\" size=\"-2\">Whois by <a href=\"mailto:partizanu@netchat.ro\">Partizanu</a></font></td></tr>";
echo "<tr><td align=\"center\"><font face=\"Arial, Helvetica, sans-serif\" size=\"-2\">Unleash &#147;<a href=\"http://www.lucas-nussbaum.net/thales/\">Thales</a>&#148; power!</font></td></tr>";
echo "</table>";
?>