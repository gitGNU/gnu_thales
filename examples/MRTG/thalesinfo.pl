#!/usr/bin/perl

# ============================================================================ 
# IRC Network usage script (powered by Thales) for mrtg 
# Copyright (C) 2003 Partizanu | partizanu@netchat.ro
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
#
#    File:	thalesinfo.pl
#    Author:    Partizanu | partizanu@netchat.ro
#    Version:	0.1
#    Input:	mySQL data from Thales tables
#    Output:	users, channels, some strings to match MRTG external script output 
#		(see http://people.ee.ethz.ch/~oetiker/webtools/mrtg/reference.html - "External Monitoring Scripts")
#
#    Make sure to:
#	1) Install DBI Perl module so you can access mySQL server (I use DBI 1.13) - http://search.cpan.org
#	2) Change $mysqluser and $mysqlpass
#	3) set permision and flush privileges in mysql so that script can connect
#	4) have fun
# ============================================================================

$mysqluser=some_sql_user;
$mysqlpass=some_sql_password;
$network=name_of_your_irc_network;

use DBI();

# 1) Get users
$dbh = DBI->connect("DBI:mysql:database=thales;host=www.netchat.ro", $mysqluser, $mysqlpass, {'RaiseError' => 1});
$q = $dbh->prepare("select count(*) as users from user");
$q->execute();
while ($answer = $q->fetchrow_hashref()) { $users = $answer->{'users'}; }
$q->finish();

# 2) Get channels
$q = $dbh->prepare("select count(*) as channels from chan");
$q->execute();
while ($answer = $q->fetchrow_hashref()) { $channels = $answer->{'channels'}; }
$q->finish();      

$dbh->disconnect();

print $users."\n";
print $channels."\n";
print "some time"."\n";
print $network ."\n";
