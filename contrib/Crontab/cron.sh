#! /bin/sh

# Thales CRONTAB script
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
