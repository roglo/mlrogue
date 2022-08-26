#!/bin/sh
# $Id: random.sh,v 1.8 2013/01/29 14:45:52 deraugla Exp $

while true; do
   ROGUEOPTS=name=$(date +"%d %b %y"),fast ../rogue -lang fr-tt -r 1000
   stty sane
   trap 'echo interrupted; exit 3' 2
   sleep 3
done
