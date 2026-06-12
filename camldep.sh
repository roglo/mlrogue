#!/bin/sh

for file in $@; do
  v=$(grep "#open" $file | sed -e 's/#open "//' -e 's/";;//' | sort)
  v=$(echo $v | sed -e "s/\n/ /")
  if test -n "$v"; then
    u=""
    for i in $v; do
      if test -f "$i.mli"; then
        u="$u $i.zi"
      elif test -f "$i.ml"; then
        u="$u $i.zo"
      fi
    done
    v="$u"
    f=$(basename "$file" ".mli")
    f=$(basename "$f" ".ml")
    if test -n "$v"; then
      fmli=$(basename "$file" ".mli")
      fml=$(basename "$file" ".ml")
      if test "$file" = "$fmli.mli"; then
        echo "$f.zi:$v"
      elif test "$file" = "$fml.ml"; then
        echo "$f.zo:$v"
      fi
    fi
  fi
done | sort
