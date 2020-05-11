#!/bin/bash

set -e

dir=$HOME/work
export PYTHONPATH=$dir/pytis/lib:$dir/lcg/lib:$dir/wiking/lib

cd $dir/pytis

if [ $(uname) == "Darwin" ]; then
   user=$USER
else
   user='postgres'
fi

# Vytvoření DB Pytis Demo:
sudo -u $user dropdb pytis-demo
sudo -u $user createdb pytis-demo
echo "create extension if not exists ltree ;" | sudo -u $user psql pytis-demo
echo "create extension if not exists plpythonu ;" | sudo -u $user psql pytis-demo

for role in demo pytis demowebuser pytiswebuser
do
   psql pytis-demo -tAc "SELECT 1 FROM pg_user WHERE usename = '$role';" | grep -q 1 || \
      sudo -u $user createuser $role && sudo -u $user psql pytis-demo -c "grant $role to $USER"
done

$dir/pytis/tools/gsql.py pytis.dbdefs.demo > pytis-demo.sql
sudo -u $user psql pytis-demo -v ON_ERROR_STOP=1 -1f pytis-demo.sql

sudo -u $user psql pytis-demo -c "insert into cms_users (login, passwd, fullname) values ('demo', md5('xxxx'), 'Demo User');"
sudo -u $user psql pytis-demo -c "insert into cms_languages (lang) values ('en');"

# Reinicializace DMP:
$dir/pytis/tools/dmp --config=pytis-demo-config.py --database=pytis-demo import
$dir/pytis/tools/dmp --config=pytis-demo-config.py --database=pytis-demo add-member $USER admin
$dir/pytis/tools/dmp --config=pytis-demo-config.py --database=pytis-demo commit
