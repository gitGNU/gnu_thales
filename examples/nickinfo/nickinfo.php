<?php

/* ********************************************************************
   nickinfo.php                                                   
   Copyright (C) 2003/2004 Andreas 'AdmiralAccident' Lindemann
   Network Administrator irc.insiderZ.DE

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


   ********************************************************************
   What this script does:

   This script will return an image (jpg) indicating if the given nickname
   is currently online or offline. Optionally it can return a     
   number (0 or 1) for further processing in other scripts.       
                                                                   
   I suggest calling this script via a html img tag                 
   <img src="http://your.host.here/nickinfo.php?nick=Your_nick&number=0" border="0" alt="" width="88" height="27"> 
   
   It is mainly meant to be integrated into forum Templates (like ICQ online/offline pics).
   You might want to replace the online/offline graphics in the img folder with something 
   more appropriate for your network :-)

   ******************************************************************** */

$dbhost = "your_db_host";
$dbuser = "your_db_user";
$dbpass = "your_db_password";
$dbname = "your_db_name";

mysql_connect($dbhost, $dbuser, $dbpass) or die("An error occurred while connecting to the database. Context: connect.");
mysql_select_db($dbname) or die("An error occurred while connecting to the database. Context: select database.");


function returnOfflinejpg() {
       $im = ImageCreateFromgif ("img/offline.gif");
       if (!$im) {                    
         die("Error while loading Image");   
       }
       Header("Content-type: image/jpeg");
       Imagejpeg($im);
}

function returnOnlinejpg() {
       $im = ImageCreateFromgif ("img/online.gif");
       if (!$im) {                    
         die("Error while loading Image");   
       }
       Header("Content-type: image/jpeg");
       Imagejpeg($im);
}

function Nickinfo($nick,$numbers) {
       $result = mysql_query("SELECT online FROM user WHERE user.nick = '$nick'")
       or die("Database query failure");
       $line = mysql_fetch_array($result, MYSQL_ASSOC);
       
       if($numbers) {
         if($line["online"] == 'Y') echo "1";
         else echo "0";
       } else {
         if($line["online"] == 'Y') returnOnlinejpg();
         else returnOfflinejpg();
       }
}


if ($number == '1') {
   Nickinfo($nick,true);
} else {
   Nickinfo($nick,false);
}
?>
