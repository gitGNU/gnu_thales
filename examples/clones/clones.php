<?php
/*  Clones detection script
 *  Copyright (C) 2002 Jens 'DukePyrolator' Voss <DukePyrolator@wiredirc.net>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* ============================================================================
 * clone-detecting-script www (powered by Thales)
 *
 *    File:      clones.php
 *    Author:    Jens 'DukePyrolator' Voss | DukePyrolator@wiredirc.de
 *    Version:   0.1
 *    Input:     mySQL data from Thales tables
 *    Output:    all clones on chan (a la mIRC)
 *    Usage:     htp://.../clones.php?chan=mychan (without the leading '#')
 *
 * ===========================================================================
 */



$start = doubleval(time() + microtime());


function safe_mysql_pconnect()
{

    $db_host = "localhost";
    $db_user = "thales";
    $db_pass = "mysecretpass";
    $db_name = "thales";
		    
    $mysql_link = mysql_connect($db_host, $db_user, $db_pass) or die("Can't connect to mysql");
    mysql_select_db($db_name);
    return($mysql_link);
}

$mysql_link = safe_mysql_pconnect();


$chan = "#$chan";
echo $chan; 
echo "<br> <br>";


$clones = mysql_query("SELECT nick, hostname
                       FROM ison,chan,user
		       WHERE channel=\"$chan\" and ison.chanid=chan.chanid and user.nickid=ison.nickid");

echo mysql_error();


$i = 1;
while ($clones_f = mysql_fetch_row ($clones)) {
      $u_nick[$i] = $clones_f[0];
      $u_host[$i] = $clones_f[1];
      $u_found[$i] = 0;
      $i = $i + 1;
};
$groesse = $i + 1;


for ($i=1;$i<$groesse;$i++) {
       $found= 0;    
       for ($j=1;$j<$groesse;$j++) {  
         if (($u_host[$i] == $u_host[$j]) AND !($i == $j) AND ($u_found[$i]==0)){
            if ($found == 0) { 
                 echo $u_nick[$i];
	         $found=1;
             }	 
             echo "    ", $u_nick[$j]; 
             $u_found[$j] = 1;
         }
       }
       if ($found == 1) { 
           echo "<br>\n";
	   $u_found[$i]=1;
       }
}



echo "\n<br><br>\n";
echo "  ", round(((time() + microtime())-$start),6), " Sek";


mysql_close($mysql_link);
?>
