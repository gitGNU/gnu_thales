#!/bin/sh
echo "generating $1.out.gz ... this can take some time !"
grep "^\[.*\] <IRC : " $1 | sed -e 's/\[.*\] <IRC : \(.*\)/\1/' | gzip > $1.out.gz
echo "$1.out.gz generated. respective sizes :"
ls -lh $1 $1.out.gz
echo "now please send $1.out.gz to lucas@lucas-nussbaum.net,"
echo "explaining how the crash occured, if possible"
echo "thanks for your help improving thales !"
