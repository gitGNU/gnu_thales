<?php

/*
 * Active channel lister with best effort HTML conversion of IRC codes in
 * topics using Lucas Nussbaum's thales package as the source of the data.  If
 * you can improve on this, please send your mods to: grifferz@blitzed.org --
 * it'd be nice to be able to support ctrl-o and better support for reverse
 * video.  An example of a slightly more customised version of this can be
 * found at http://blitzed.org/channels.phtml (http://blitzed.org/channels.phps
 * for source)
 *
 * Copyright 2002 the Blitzed IRC Network All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *     1.  Redistributions of source code must retain the above copyright
 *         notice, this list of conditions and the following disclaimer.
 *
 *     2.  Redistributions in binary form must reproduce the above
 *         copyright notice, this list of conditions and the following
 *         disclaimer in the documentation and/or other materials provided
 *         with the distribution.
 *     3.  Neither the name of the Blitzed IRC Network, nor the names of
 *         its contributors may be used to endorse products derived from
 *         this work without specific prior written permission.
 *
 * See the Blitzed Software License for more details, a copy of which
 * should have been provided with this software in the file "COPYING". If
 * not, a copy may be obtained from
 * http://www.blitzed.org/docs/license.phtml
 */

/*
 * Connect to MySQL. (I usually put this function in another file outside
 * the web space, due to the fact that it has a password in it.  It is
 * provided only as an example.)
 */
function safe_mysql_pconnect($db)
{
	$mysql_link = mysql_connect("localhost", "user", "secretpassword")
	    or die("Can't connect to mysql");
	mysql_select_db($db);
	return($mysql_link);
}
	
/*
 * Try to bold everything between two IRC bold codes, underline everything
 * between two IRC underline codes, and then parse colours.
 */
function htmlify($string)
{
	$ctrlc = "\003";
	$bold = "\002";
	$uline = "\037";
	$rv = "\026";

	if (!string) {
		return "&nbsp;";
	}

	/*
	 * It is very important that HTML entities in the channel topic
	 * are removed otherwise you WILL be amazed how much javascript
	 * badness can be fitted into a channel topic.
	 */
	$string = htmlentities($string);

	if (preg_match("/$ctrlc/", $string)) {
		$string = colourise($string);
	}

	$string = preg_replace("/$bold(.*?)$bold/",
	    "<span style='font-weight: bold;'>$1</span>", $string);
	$string = preg_replace("/$uline(.*?)$uline/",
	    "<span style='text-decoration: underline;'>$1</span>", $string);
	/* I am goping to cheat and just put white on black. */
	$string = preg_replace("/$rv(.*?)$rv/",
	    "<span style='color: white; background-color: black'>$1</span>",
	    $string);

	return($string);
}

/*
 * Parse out IRC colour codes into HTML.  There's no way to make this
 * perfect, since even IRC clients differ in how they handle some of this
 * syntax.  It is also really easy for people to make broken HTML appear
 * here, so this page is not going to validate.
 */
function colourise($string)
{
	$ctrlc = "\003";
	$delim = "\011";

	/* Colours by mIRC number */
	$colours[0] = "#ffffff"; // white
	$colours[1] = "#000000"; // black
	$colours[2] = "#00007F"; // dkblue
	$colours[3] = "#009000"; // dkgreen
	$colours[4] = "#ff0000"; // red
	$colours[5] = "#7F0000"; // dkred
	$colours[6] = "#9F009F"; // purple
	$colours[7] = "#FF7F00"; // orange
	$colours[8] = "#FFFF00"; // yellow
	$colours[9] = "#00F800"; // higreen
	$colours[10] = "#00908F"; // teal
	$colours[11] = "#00FFFF"; // cyan
	$colours[12] = "#0000ff"; // blue
	$colours[13] = "#ff00ff"; // magenta
	$colours[14] = "#7F7F7F"; // dkgrey
	$colours[15] = "#CFD0CF"; // ltgrey

	/* Need to put a "guard" character in which we'll split on .*/
	$string = preg_replace("/$ctrlc/", "$ctrlc$delim", $string);

	/* split it out into parts. */
	$s = explode($ctrlc, $string);

	$new_s = "";

	/* process each piece. */
	foreach ($s as $i) {
//		echo "<pre>TOKEN: $i</pre>\n";
		/* Parse the numbers out.  Try for 2 numbers first. */
		if (preg_match("/$delim(\d+),(\d+)/", $i, $m)) {
			$c1 = $m[1];
			$c2 = $m[2];

//			echo "<pre>GOT COLOURS: $c1 $c2</pre>\n";
			/* Replace with HTML. */
			$i = preg_replace("/$delim$c1,$c2(.*)/",
			    "<span style='color: $colours[$c1]; " .
			    "background-color: $colours[$c2];'>$1</span>",
			    $i);
		} else if (preg_match("/$delim(\d+)/", $i, $m)) {
			$c1 = $m[1];

			$i = preg_replace("/$delim$c1(.*)/",
			    "<span style='color: $colours[$c1];'>$1</span>",
			    $i);
		}

		/* Get rid of those delims if any are left. */
		$i = preg_replace("/$delim/", "", $i);
//		echo "<pre>DONE TOKEN: $i</pre>\n";
		$new_s[] = $i;
	}

	/* Put it back together. */
	$string = join("", $new_s);
//	echo "<pre>NEW: $string</pre>\n";
	return($string);
}

/*
 * Turn things that look like urls into real links.  This came from The
 * Perl Cookbook, I've translated it to PHP.
 */
function urlify($string)
{
	$urls = '(http|telnet|ftp|https)';
	$letters = '\w';
	$gunk = '\/\#~|.?+=&%@!\-';
	$punctuation = ';.:?\-';
	$any = "$letters$gunk$punctuation";
	
	$string = preg_replace(
	    "/\b($urls:[$any]+?)(?=[$punctuation]*[^$any]|$)/ix",
	    "<a href='$1'><span style='font-face: monospace'>$1</span></a>",
	    $string);
	return($string);
}


?>
<html>
<head>
<title>Active Channels</title>
<link rel='stylesheet' href='example.css' media='all' />
</head>
<body>
<h1>Currently Active Channels</h1>

<p>This is an automatically generated list of channels on the <a
href="/">Random IRC Network</a>.  If you are using mIRC, or any other client
which supports the <tt>irc://</tt> protocol, then clicking on the mIRC icon
will make your client join the chosen channel on Blitzed.  Otherwise, clicking
on the channel name will load our Java chat client.  Happy chatting!</p>

<table class="chanList">
<tr align="left"><th>&nbsp;</th><th>Channel Name</th><th>Users</th><th>Topic</th></tr>

<?php

$mysql_link = safe_mysql_pconnect("thales");
$mysql_res = mysql_query("SELECT chan.channel, COUNT(*) AS users, " .
    "chan.topic FROM chan, ison WHERE chan.chanid=ison.chanid && " .
    "chan.mode_ls='N' && chan.mode_lp='N' GROUP BY channel " .
    "ORDER BY users DESC");

If (mysql_errno()) {
	echo "<pre>ERROR: " . mysql_error() . "</pre>\n";
	exit;
}

$i = 0;

while ($mysql_row = mysql_fetch_row($mysql_res)) {
	/*
	 * This causes the rows to alternate styles which I find easier to
	 * read.
	 */
	if ($i == 0) {
		$td = "<td class='chanlistrow1'>";
		$i = 1;
	} else {
		$td = "<td>";
		$i = 0;
	}

	/* Escape weird characters in channel name */
	$chan = htmlentities($mysql_row[0]);

	/*
	 * And the # symbol which HTML validation services don't seem to
	 * like.
	 */
	$chan = preg_replace("/#/", "&#35;", $chan);

	/*
	 * We need a short channel name (without leading #) for various
	 * purposes.
	 */
	$shortchan = preg_replace("/^&#35;/", "", $chan);

	/* User count for the channel */
	$users = $mysql_row[1];

	/*
	 * Try as best we can to accurately replicate colour codes, bolds,
	 * underline and urls.  It is not possible to do this perfectly.
	 */
	$topic = htmlify($mysql_row[2]);
	$topic = urlify($topic);

	/* Start displaying the row */
	echo "<tr>\n";
	echo "$td<p>\n";

	/*
	 * Dinky mIRC links.  If you are lucky, clicking on these will make
	 * mIRC open, connect to your network and join the correct channel.
	 * I've noticed it does not work that reliably though, especially
	 * if you don't use Windows and IE.
	 */
	echo "<a href='irc://irc.blitzed.org:6667/$shortchan'>\n";
	echo "<img src='/images/mirc_small.png' width='16' height='16' " .
	    "alt='Chat now!' />\n";
	echo "</a>\n";
	echo "</p></td>\n";

	/*
	 * Link to our EIRC java chat (http://eirc.sf.net) to get them
	 * straight into the channel that way.
	 */
	echo "$td<p>\n";
	echo "<a href='/java/chat.phtml?chan=$shortchan'>$chan</a>\n";
	echo "</p></td>\n";

	/* User count. */
	echo "$td<p>\n";
	echo "$users\n";
	echo "</p></td>\n";

	/* The topic. */
	echo "$td<p>\n";
	echo "$topic\n";
	echo "</p></td>\n";

	/* And that is it. */
	echo "\n\n";
}

?>

</table>
</body>
</html>

