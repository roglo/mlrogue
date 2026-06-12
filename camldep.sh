#!/bin/sh

for file in $@; do
  v=$(grep "#open" $file | sed -e 's/#open "//' -e 's/";;//')
  w=$(grep __ $file | sed -e 's/__.*$/__/' | rev | sed -e 's/__\([a-z_]*\).*$/\1/' | rev)
  w=$(echo $v $w | sed -e 's/ /\\n/g')
  v=$(echo $w | sort | uniq)
  f=$(basename "$file" ".mli")
  f=$(basename "$f" ".ml")
  fmli=$(basename "$file" ".mli")
  fml=$(basename "$file" ".ml")
echo $file
echo $v
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
    if test -n "$v"; then
      if test "$file" = "$fmli.mli"; then
        echo "$f.zi:$v"
      elif test "$file" = "$fml.ml"; then
        echo -n "$f.zo:"
        if test "$file" = "$fml.ml" -a -f $fml.mli; then
          echo -n " $f.zi"
        fi
        echo "$v"
      fi
    fi
  elif test "$file" = "$fml.ml" -a -f $fml.mli; then
    echo "$f.zo: $f.zi"
  fi
done | sort
