<?php

//informations
$db_host = "localhost";
$db_user = "login";
$db_pass = "password";
$db_name = "database";




//on va chercher le nombre d'users

//compteur de chans ?
$nbchan = ($_REQUEST['type'] == "nbchan");

//nom du chan
$chan = '#'.strtolower($_REQUEST['chan']);

//mode texte ?
$texte = $_REQUEST['texte'];

//couleurs
$fg = $_REQUEST['fg'];
$bg = $_REQUEST['bg'];
if ($fg == "") $fg = 0;
if ($bg == "") $bg = 0xFFFFFF;

//fond
$rgb[0]=intval($bg/0x10000);
$rgb[1]=intval(($bg/0x100)/0x100);
$rgb[2]=intval($bg%0x100);

//ecriture
$rgb[3]=intval($fg/0x10000);
$rgb[4]=intval(($fg/0x100)/0x100);
$rgb[5]=intval($fg%0x100);

if ($chan != "#") $requete = "SELECT COUNT(*) FROM user, ison, chan WHERE user.nickid = ison.nickid AND ison.chanid = chan.chanid AND channel = \"$chan\"";
else if ($nbchan) $requete = "SELECT COUNT(*) FROM chan;";
else $requete = "SELECT COUNT(*) FROM user;";


//connexion à la base de données
$mysql_link = mysql_connect($db_host, $db_user, $db_pass) or die("Probleme avec la base de données");
mysql_select_db($db_name) or die("Probleme avec la base de données (A)");

$nbu = mysql_query($requete) or die("Probleme avec la base de données (D)");
$nb = mysql_fetch_array ($nbu) or die ("Probleme avec la base de données (E)");
$compteur = $nb[0];

if ($texte == "") {
	$font=5; /*font de 1 à 5 */
	$largeur_compteur=strlen($compteur);
	$largeur_font=imagefontwidth($font);
	$hauteur_font=imagefontheight($font);
	$largeur_image=($largeur_compteur+2)*$largeur_font;
	$hauteur_image=$hauteur_font*2;

	/* envoie du code mime dans le header */
	header ("Content-type: image/png");

	$image = @imagecreate ($largeur_image, $hauteur_image) or die ("Impossible d'initialiser la librairie GD");

	$background_color =imagecolorallocate ($image, $rgb[0],$rgb[1],$rgb[2]);
	$text_color = imagecolorallocate ($image, $rgb[3],$rgb[4],$rgb[5]);

	imagerectangle ($image,0,0,$largeur_image-1,$hauteur_image-1, $text_color);
	imagestring ($image, $font, $largeur_font, $hauteur_font/2, "$compteur ", $text_color);

	imagepng ($image);
}
else {
	echo $compteur;
}

mysql_close($mysql_link);
?>
