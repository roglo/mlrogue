#!/bin/sh
# $Id: trace_strace.sh,v 1.4 2010/05/31 19:45:07 deraugla Exp $

strace -p $1 -e write=1 2>&1 | grep --line-buffered '^ |' |
sed -u -e 's/^ | .....  //; s/..................|$//' |
awk '{for (i = 1; i <= NF; i++) printf "%c", strtonum("0x" $i); fflush()}'
