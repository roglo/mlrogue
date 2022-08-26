#!/bin/sh -e
# $Id: random_diff.sh,v 1.1 2010/04/11 23:27:13 deraugla Exp $

while true; do
  i=$(../../factor/random 10)
  echo $i > i
  ROGUEOPTS=name=$(date +"%Y-%m-%d")r,noskull,fast ../rogue -lang fr -r 1000,t,n -seed $i 2>toto
  ROGUEOPTS=name=$(date +"%Y-%m-%d")r,noskull,fast ../rogue -lang en -r 1000,t,n -seed $i 2>titi
  cmp toto titi
done
