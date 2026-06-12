#!/bin/sh

for file in "$@"; do
  v=$(grep "#open" $file | sed -e 's/#open "//' -e 's/";;//')
#  v=$(echo $v | sed -e "s/\n/aaaaaaaaa/g")
  f=$(basename "$file" ".mli")
  f=$(basename "$f" ".ml")
  if test -n "$v"; then
    v=$(echo $v | sed -e "s/ /.zi /g" -e 's/$/.zi/')
    fmli=$(basename "$file" ".mli")
    fml=$(basename "$file" ".ml")
    if test "$file" = "$fmli.mli"; then
      echo "$f.zi: $v"
    elif test "$file" = "$fml.ml"; then
      echo "$f.zo: $v"
    fi
  fi
done
