#!/bin/bash

DIRNAME=$(dirname $0)
SRCDIR=$DIRNAME/../lib
if [ ! -d $SRCDIR/pytis ]; then
  echo 'Source directory not found'
  exit -1
fi

cd $SRCDIR
echo "*** $(pwd) ***"
find -type f \( -name '*.py' -o -name 'ebas*' \) \
     -not -name 'test.py' -not -name '_config.py' | \
  xargs -r egrep -l '^[^#]*\<print '
