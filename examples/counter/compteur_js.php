<?
//-----------------------------------------------------------------------------
# compteur_js.php - Web page counter for Thales
# Copyright (C) 2003 Hervé Rousseau <z-master@apinc.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program (in docs/LICENSE); if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Little hack from digits2img script by me [ z-master@apinc.org ]
# Contact me for bug, requests etc...
//-----------------------------------------------------------------------------

/* MySQL Configuration */
$host = "localhost";
$user = "lucas";
$pass = "X";
$bdd = "thales";
function writenumbers($number,$funcset)
	{
		//Number of digits to show
		$digits = 3;
		//The "webpath" to the pictures/ directory
		$path = "http://www.dwchat.net/tools/thales/compteur/" ;
		
		$img = NULL;
		if (!is_numeric($funcset)) return $img;
		if (is_numeric($number))
			{
				$number = str_pad($number, $digits, "0", STR_PAD_LEFT);
				for ($k = 0; $k < $digits;$k++)
					{
						$tmp = substr($number,$k,1);
						$img = $img . "<img src=\"".$path."".$funcset."/".$tmp.".png\" title=\"".$number."\">";
					}
			}
		else return $img;
	return $img;
	}

/* MySQL Connection*/
$link = mysql_connect ("$host", "$user", "$pass") or die ("Unable to connect to MySQL Server");
mysql_select_db("$bdd");

/* The request */
$q = mysql_query("select count(*) users from chan, ison where chan.chanid = ison.chanid and channel=\"#$chan\"");

/* We get back the results */
$r = mysql_result($q,"users");

/* And make the output */

if(!$skin)
{
	/* Text output */
	print("document.write('$r');");
	print("\n");
}
else
{
	/* Images output */
	$img_tags = writenumbers($r,$skin,$chan);
	//echo("<!-- Thales IRC UserCounter -->\n");
	print("document.write('$img_tags');");
//	echo "\n<br>";
}

/* That's all */

mysql_close($link);
?>
