#!/bin/sh
# logfilter - strips thales logs
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

echo "generating $1.out.gz ... this can take some time !"
grep "^\[.*\] <IRC : " $1 | sed -e 's/\[.*\] <IRC : \(.*\)/\1/' | gzip > $1.out.gz
echo "$1.out.gz generated. respective sizes :"
ls -lh $1 $1.out.gz
echo "now please send $1.out.gz to lucas@lucas-nussbaum.net,"
echo "explaining how the crash occured, if possible"
echo "thanks for your help improving thales !"