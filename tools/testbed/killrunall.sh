#!/bin/sh

DISTS='bahamut hybrid unreal ultimate28 ultimate30'
TESTBED='/home/lucas/testbed'

# kill all thales

ps x |grep thales |grep -v grep | awk '{print $1}' | xargs kill

for j in $DISTS; do
	for k in $TESTBED/$j/*; do
		echo $k
		cd $k
		if [ -f thales.log ]; then
			mv -f thales.log thales.log.`date +%s`
		fi
		./thales -v
	done
done
