#!/bin/sh

DISTS='bahamut hybrid unreal ultimate28 ultimate30'
TESTBED='/home/lucas/testbed'
MAKE=gmake
# compile

for j in $DISTS; do
	mkdir -p $TESTBED/$j
	for k in $TESTBED/$j/*; do
		./configure --with-ircd=$j --with-tableprefix=`basename $k`_ --enable-noreportusage
		$MAKE
		mv -f src/thales $k/thales
	done
done

echo "done !"
