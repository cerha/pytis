#!/bin/bash

set -e

if [ $(uname) == "Darwin" ]; then
   user=$USER
else
   user='postgres'
fi

# Vytvoření DB Pytis Demo:

sudo -u $user dropdb pytis-demo --if-exists

sudo -u $user createdb pytis-demo
echo "create extension if not exists ltree ;" | sudo -u $user psql pytis-demo
echo "create extension if not exists plpython3u ;" | sudo -u $user psql pytis-demo

for role in pytis demo pytis-demo demowebuser pytiswebuser www-data
do
   psql pytis-demo -tAc "SELECT 1 FROM pg_user WHERE usename = '$role';" | grep -q 1 || \
      sudo -u $user createuser $role && sudo -u $user psql pytis-demo -c "grant \"$role\" to $USER"
done

tools/gsql.py --plpython3 pytis.dbdefs.demo > pytis-demo.sql
sudo -u $user psql pytis-demo -v ON_ERROR_STOP=1 -1f pytis-demo.sql

sudo -u $user psql pytis-demo -c "insert into cms_users (login, passwd, fullname) values ('demo', md5('xxxx'), 'Demo User');"
sudo -u $user psql pytis-demo -c "insert into cms_languages (lang) values ('en');"

# Reinicializace DMP:
tools/dmp --config=pytis-demo-config.py --database=pytis-demo import
tools/dmp --config=pytis-demo-config.py --database=pytis-demo add-member $USER admin
tools/dmp --config=pytis-demo-config.py --database=pytis-demo commit
