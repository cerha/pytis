# -*- coding: utf-8 -*-

# Copyright (C) 2011, 2012 Brailcom, o.p.s.
#
# COPYRIGHT NOTICE
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Gensql definitions for database column encryption support."""

db_rights = globals().get('Gall_pytis', None)
if not db_rights:
    raise ProgramError('No rights specified! Please define Gall_pytis')

execfile('db_pytis_crypto_basic.py', copy.copy(globals()))

## Optional key management

_std_table('c_pytis_crypto_names',
           (P('name', TString),
            C('description', TString),
            ),
           grant=db_rights,
           doc="Codebook of encryption areas defined in the application.",
           depends=())

_std_table('e_pytis_crypto_keys',
           (P('key_id', TSerial),
            C('name', TString, constraints=('not null',),
              references='c_pytis_crypto_names on update cascade'),
            C('username', TString, constraints=('not null',),
              doc="Arbitrary user identifier."),
            C('key', 'bytea', constraints=('not null',)),
            C('fresh', TBoolean, constraints=('not null',), default='false',
              doc=("Flag indicating the key is encrypted by a non-login password. ")),
            ),
           sql='unique (name, username)',
           grant=db_rights,
           doc="Table of encryption keys of users for defined encryption areas.",
           depends=('c_pytis_crypto_names',))

viewng('ev_pytis_user_crypto_keys',
       (R('e_pytis_crypto_keys', alias='keys', exclude_columns=('username', 'key',),
          condition="username=current_user"),),
       insert=None,
       update=None,
       delete=None,
       grant=db_rights,
       depends=('e_pytis_crypto_keys',)
       )

sql_raw("""
create or replace function pytis_crypto_extract_key (encrypted bytea, psw text) returns text as $$
declare
  key text;
begin
  begin
    key := pgp_sym_decrypt(encrypted, psw);
  exception
    when OTHERS then
      return null;
  end;
  if substring(key for 6) != 'pytis:' then
    return null;
  end if;
  return substring(key from 7);
end;
$$ language plpgsql immutable;

create or replace function pytis_crypto_store_key (key text, psw text) returns bytea as $$
-- This a PL/pgSQL, and not SQL, function in order to prevent direct dependency on pg_crypto.
begin
  return pgp_sym_encrypt('pytis:'||$1, $2);
end;
$$ language plpgsql;

create or replace function pytis_crypto_insert_key (name_ text, user_ text, key_ text, psw text) returns bool as $$
begin
  lock e_pytis_crypto_keys in exclusive mode;
  if (select count(*) from e_pytis_crypto_keys where name=name_ and username=user_) > 0 then
    return False;
  end if;
  insert into e_pytis_crypto_keys (name, username, key) values (name_, user_, pytis_crypto_store_key(key_, psw));
  return True;
end;
$$ language plpgsql;

create or replace function pytis_crypto_change_password (id_ int, old_psw text, new_psw text) returns bool as $$
declare
  key_ text;
begin
  lock e_pytis_crypto_keys in exclusive mode;
  begin
    select pytis_crypto_extract_key(key, $2) into key_ from e_pytis_crypto_keys where key_id=$1;
  exception
    when OTHERS then
      key_ := null;
  end;
  if key_ is null then
    return False;
  end if;
  update e_pytis_crypto_keys set key=pytis_crypto_store_key(key_, $3), fresh=False where key_id=$1;
  return True;
end;
$$ language plpgsql security definer;

create or replace function pytis_crypto_copy_key (name_ text, from_user text, to_user text, from_psw text, to_psw text) returns bool as $$
declare
  key_ text;
begin
  lock e_pytis_crypto_keys in exclusive mode;
  begin
    select pytis_crypto_extract_key(key, from_psw) into key_ from e_pytis_crypto_keys where name=name_ and username=from_user;
  exception
    when OTHERS then
      key_ := null;
  end;
  if key_ is null then
    return False;
  end if;
  delete from e_pytis_crypto_keys where name=name_ and username=to_user;
  insert into e_pytis_crypto_keys (name, username, key, fresh)
         values (name_, to_user, pytis_crypto_store_key(key_, to_psw), True);
  return True;
end;
$$ language plpgsql;

create or replace function pytis_crypto_delete_key (name_ text, user_ text, force bool) returns bool as $$
begin
  lock pytis_crypto_keys in exclusive mode;
  if not force and (select count(*) from pytis_crypto_keys where name=name_) <= 1 then
    return False;
  end if;
  delete from e_pytis_crypto_keys where name=name_ and username=user_;
  return True;
end;
$$ language plpgsql;
""",
        name='pytis_crypto_key_functions',
        depends=('e_pytis_crypto_keys', 'pytis_basic_crypto_functions',))

## Using login names and passwords for key encryption

sqltype('pytis_crypto_t_user_contact',
        (C('email', TString),
         C('gpg_key', TString),
         ))

sql_raw("""
create or replace function pytis_crypto_unlock_passwords (user_ text, password_ text) returns setof text as $$
begin
  lock e_pytis_crypto_keys in exclusive mode;
  begin
    delete from t_pytis_passwords;
  exception
    when undefined_table then
      create temp table t_pytis_passwords (name text, password text);
  end;
  insert into t_pytis_passwords
         (select name, pytis_crypto_extract_key(key, password_)
                 from e_pytis_crypto_keys
                 where username=user_ and pytis_crypto_extract_key(key, password_) is not null);
  return query select name from t_pytis_passwords;
end;
$$ language plpgsql;

create or replace function pytis_crypto_unlock_current_user_passwords (password_ text) returns setof text as $$
select * from pytis_crypto_unlock_passwords(current_user, $1);
$$ language sql;

-- create function pytis_crypto_user_contact (username text) returns pytis_crypto_t_user_contact as ...
""",
        name='pytis_login_key_functions',
        depends=('e_pytis_crypto_keys', 'pytis_basic_crypto_functions',))
