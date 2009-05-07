#!/bin/bash

DIRNAME=$(dirname $0)
BASEDIR=$DIRNAME/..
if [ ! -d $BASEDIR/lib/pytis ]; then
  echo 'Source directory not found'
  exit -1
fi

cd $BASEDIR
rm -f TAGS
find lib/pytis -name '*.py' -not -name '_test.py' | xargs etags --append --regex='/^[ \t]+def[ \t]+\([a-zA-Z_0-9]+\)/\1/'
