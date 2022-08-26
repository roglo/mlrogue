#!/bin/sh
# $Id: scan_seeds.sh,v 1.14 2013/01/29 14:45:52 deraugla Exp $

i=$1

while true; do
   echo $i > i
   ROGUEOPTS=name=$(date +"%d %b %y"),fast ../rogue -lang fr-tt -r 1000,p -seed $i
   stty sane
   trap 'echo interrupted; exit 3' 2
   echo seed was $i
   i=$((i+1))
   sleep 3
done
