<?php
/* UnrealCloak (3.1.x) over PHP by Magnet -- magnet@epiknet.org
 *
 * First made out (13/04/2002) for Thales (software by lucas -- lucas@lucas-nussbaum.net)
 * Thales : IRC to SQL Gateway -- http://www.lucas-nussbaum.net/thales/
 * EpiKnet : French IRC Network -- http://www.epiknet.org
 *
 * Thanks goes to 
 *		-MrJul for Unreal Cloaking mIRC script (I merged a port of this mIRC script and the original Unreal src/cloak.c file by the UnrealIRCd dev team)
 *		-Kevin for some PHP help (yeah, the first element of an array is always 0 in PHP :p)
 *		-lucas for giving me the good idea of porting the Unreal 3.1.x Cloaking to PHP
 *
 *
 * Use: just require the file to the PHP script you're using ( require("cloak.php"); ) then call the function u_cloak().
 * params are $prefix (your network cloaking prefix) and the realhost ($rhost)
 *
 * If you uncomment the last line, this script can be used by web too (just run the script with params prefix and host)
 *
 * Example: function use : u_cloak("EpiK","www.epiknet.org") returns EpiK-60365.epiknet.org
 * 			web use : http://www.epiknet.org/cloak.php?prefix=EpiK&host=www.epiknet.org will echo EpiK-60365.epiknet.org
 *
 * Misc: 
 *		-**UNREAL3.1.x CLOAKING IS NOT REVERSIBLE** All you can do is brute force cracking, and while some hostnames may be found out, some
 * 		are just unbreakable.
 *
 *		-Unreal3.1.x Cloaking is case sensitive. u_cloak("EpiK","ANantes-102-1-3-58.abo.wanadoo.fr") returns EpiK-14457.abo.wanadoo.fr while u_cloak("EpiK","anantes-102-1-3-58.abo.wanadoo.fr") returns EpiK-14617.abo.wanadoo.fr
 *
 *		-This script may be used by anyone respecting the internet netiquette. If you are a spammer, a thief, or some kind of lamer, you are
 *		using this piece of software illegally. And being a lamer, you don't care, of course :)... 
 */


function IsIP($rhost) {
	$i = 0;
	$host = explode(".", $rhost);
	if (sizeof($host) != 4) return FALSE;
	while ($i < 4) {
		if (!is_numeric($host[$i]))
			return FALSE;
		if (($host[$i] < 0) or ($host[$i] > 255))
			return FALSE;
		$i++;
	}
	return TRUE;
}


function MaskChecksum($rhost) {
	$i = 0;
	$j = 0;
	for ($i = 0; $i < strlen($rhost); $i++)
	{
		$j += ord($rhost[$i]) * ($i < 16 ? ($i + 1) * ($i + 1) : $i * ($i - 15));
	}
	return ($j + 1000) % 0xffff;
}


function u_cloak($prefix, $rhost) {
	$lsum = MaskChecksum($rhost);
	$hpart = explode(".", $rhost);
	if (IsIP($rhost)) {
		$hpart[(sizeof($hpart)-1)] = $prefix . "-" . $lsum;
	}
	else {
		if (sizeof($hpart) == 1) {
			return $rhost . "-" . $lsum;
		}
		elseif (sizeof($hpart) == 2) {
			return $prefix . "-" . $lsum . "." . $rhost;
		}
		else {
			$hpart[0] = $prefix . "-" . $lsum;
		}
	}
	return implode(".",$hpart);
}

//Uncomment the line below to activate the web cloaking.
//echo u_cloak($prefix,$host);

?>