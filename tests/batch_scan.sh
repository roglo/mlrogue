#!/bin/sh -e
# $Id: batch_scan.sh,v 1.1 2010/03/22 10:40:27 deraugla Exp $

i=$1

while true; do
   echo $i
   ROGUEOPTS=name=robot,noskull,fast ./rogue -lang fr-tt -r 1000,p -seed $i -b
   i=$((i+1))
done
