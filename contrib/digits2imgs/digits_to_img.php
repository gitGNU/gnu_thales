<?
//-------------------------------------------------------------------------------------
// Generic function to transform digits to <img> tags
// Variables to customize befor running the function:
// 	[$digits] - How many digits to expect (useful to convert (let's say) 34 to 034 )
//	[$path] - location of the sets of images
//	[function writenumbers($number,$funcset=1)] - $funcset is the default set of images to use (here seted to 1)
//
// Each set is expected to contain 10 gifs named: 0.gif, 1.gif, 2.gif ...9.gif
//
// Given the following structure:
//       /-www (website root dir)
//          |-images
//               |-sets
//                  |-1
//                  |-2
//                  |-3
//
// We can define: $path = "http://www.mysite.com/images/sets/"; $funcset=2;
//
// You can download sets of images from sites as http://www.counterart.com
//-------------------------------------------------------------------------------------


function writenumbers($number,$funcset=1)
	{
		//REDEFINE THIS!
		$digits = 4;
		//REDEFINE THIS!
		$path = "http://www.netchat.ro/pita/img/" ;
		
		$img = NULL;
		if (!is_numeric($funcset)) return $img;
		if (is_numeric($number))
			{
				$number = str_pad($number, $digits, "0", STR_PAD_LEFT);
				for ($k = 0; $k < $digits;$k++)
					{
						$tmp = substr($number,$k,1);
						$img = $img . "<img src=\"".$path."".$funcset."/".$tmp.".gif\">";
					}
			}
		else return $img;
	return $img;
	}


//-------------------------------------------------------------------------------------
// Example of usage with thales
//-------------------------------------------------------------------------------------


//do MySQL connect
$link = mysql_connect ("localhost", "php", "phprulez") or die ("Can't connect");

//select Thales database
mysql_select_db("thales");

//fire query
$q = mysql_query("select count(*) users from chan, ison where chan.chanid = ison.chanid and channel=\"#kappa\"");

//get result
$r = mysql_result($q,"users");

//print number
echo $r;
echo "\n<hr>";

//get IMG tags with function's default set (of images)
$img_tags = writenumbers($r);
echo $img_tags;
echo "\n<hr>";

//get IMG tags with a given set  (of images)
$img_tags = writenumbers($r,3);
echo $img_tags;
echo "\n<br>";

//close connection
mysql_close ($link);

//-------------------------------------------------------------------------------------
// Example of usage with any given number;
//-------------------------------------------------------------------------------------
echo "\n<br>";
echo writenumbers(111,6);
?>