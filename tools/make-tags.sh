#!/bin/bash

DIRNAME=$(dirname $0)
SRCDIR=$DIRNAME/../lib
if [ ! -d $SRCDIR/pytis ]; then
  echo 'Source directory not found'
  exit -1
fi

cd $SRCDIR
rm -f TAGS
find -name '*.py' -not -name '_test.py' | xargs etags --append --regex='/^[ \t]+def[ \t]+\([a-zA-Z_0-9]+\)/\1/'
