#!/bin/sh
# $Id: random3.sh,v 1.5 2013/01/29 14:45:52 deraugla Exp $

while true; do
   i=$(../../factor/random 9)
   echo $i > i
   ROGUEOPTS=name=$(date +"%d %b %y"),noskull,fast ../rogue -lang fr-tt -r 1000,p -seed $i
   stty sane
   trap 'echo interrupted; exit 3' 2
   echo seed was $i
   sleep 3
done
