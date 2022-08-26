#!/bin/bash
INPUT=rogue.log
 
while IFS= read -r -n1 char; do
  if test "$char" = ""; then sleep 0.01; fi
  echo  -n "$char"
done < "$INPUT"
