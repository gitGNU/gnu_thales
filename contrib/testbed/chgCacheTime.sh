#!/bin/sh

TESTBED='/home/lucas/testbed'

# New Cache Times
SRVCACHE=600
USRCACHE=170

for i in `find $TESTBED/* |grep thales.conf`; do
	sed "s/^ServerCacheTime.*$/ServerCacheTime $SRVCACHE/" $i > $i.bak
	sed "s/^UserCacheTime.*$/UserCacheTime $USRCACHE/" $i.bak > $i
	rm -f $i.bak
done
