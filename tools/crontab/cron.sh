#! /bin/sh
# Thales crontab script
# Copyright (C) 2002 Daniele Nicolucci <jollino@sogno.net>
# Copyright (C) 2002 Lucas Nussbaum <lucas@lucas-nussbaum.net>
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Written by Jollino <jollino@sogno.net>
# Improved by Lucas <lucas@lucas-nussbaum.net>

# CONFIGURATION

thales_dir='/home/lucas/thales'
# you might want to add the -v parameter to generate more output
thales_exec='./thales'
thales_pid='thales.pid'
thales_log='thales.log'

# SCRIPT

cd $thales_dir
if [ -f $thales_pid ]; then
	pid=`cat $thales_pid`
	if [ `ps -p $pid | wc -l` -eq 2 ]; then
		exit
	else
		echo "Pidfile was stale."
	fi
fi

echo "Looks like Thales died... let's relaunch it"
backuplogname=$thales_log.`date '+%s'`
mv $thales_log $backuplogname
gzip $backuplogname
$thales_exec
