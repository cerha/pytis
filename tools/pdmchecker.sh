#!/bin/bash

DIRNAME=$(dirname $0)
SRCDIR=$DIRNAME/../lib
echo "$SRCDIR"
if [ ! -d $SRCDIR/pytis ]; then
  echo 'Source directory not found'
  exit -1
fi

cd $SRCDIR
echo "*** $(pwd) ***"
BADFILES=$(find -type f -name '*.py' -not -name '_*' | \
           xargs -r egrep -l '.{80,}')

if [ -n "$BADFILES" ]; then
  echo 'Warning!! The following files contain too long lines, pdm is gonna be mad!'
  for F in $BADFILES; do
    echo $F
  done
fi
