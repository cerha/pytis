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

def include_file(filename):
    import imp
    file_, pathname, description = imp.find_module(filename)
    include(pathname, globals())

include_file('db_pytis_crypto_basic')
    
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
  plain_old_psw text := pytis_crypto_decrypt_db_password(old_psw, 'pytis');
  plain_new_psw text := pytis_crypto_decrypt_db_password(new_psw, 'pytis');
begin
  lock e_pytis_crypto_keys in exclusive mode;
  begin
    select pytis_crypto_extract_key(key, plain_old_psw) into key_ from e_pytis_crypto_keys where key_id=id_;
  exception
    when OTHERS then
      key_ := null;
  end;
  if key_ is null then
    return False;
  end if;
  update e_pytis_crypto_keys set key=pytis_crypto_store_key(key_, plain_new_psw), fresh=False where key_id=id_;
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
  lock e_pytis_crypto_keys in exclusive mode;
  if not force and (select count(*) from e_pytis_crypto_keys where name=name_) <= 1 then
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

table('pytis_crypto_db_keys',
      (P('key_name', TString),
       C('public', TString),
       C('private', TString),
       ),
      doc="""
Table of asymetric encryption keys.
It is currently used to encrypt user passwords passed to some database functions.
Use select pytis_crypto_create_db_key('pytis', 1024) to create a key for that purpose.
""")

sqltype('pytis_crypto_t_key_pair',
        (C('public', TString),
         C('private', TString),
         ))

def pytis_crypto_generate_key(bits):
    bits = args[0]
    import Crypto.PublicKey.RSA
    rsa = Crypto.PublicKey.RSA.generate(bits)
    public = rsa.publickey().exportKey()
    private = rsa.exportKey()
    return [public, private]
_plpy_function('pytis_crypto_generate_key', (TInteger,), RT('pytis_crypto_t_key_pair'),
               body=pytis_crypto_generate_key,
               depends=())

def pytis_crypto_decrypt_using_key(private, encrypted):
    private, encrypted = args
    import Crypto.PublicKey.RSA, base64
    rsa = Crypto.PublicKey.RSA.importKey(private)
    return rsa.decrypt(base64.decodestring(encrypted))
_plpy_function('pytis_crypto_decrypt_using_key', (TString, TString,), TString,
               body=pytis_crypto_decrypt_using_key,
               depends=())

def pytis_crypto_encrypt_using_key(public, text):
    public, text = args
    import Crypto.PublicKey.RSA, base64
    rsa = Crypto.PublicKey.RSA.importKey(public)
    encrypted = rsa.encrypt(text, None)[0]
    return base64.encodestring(encrypted)
_plpy_function('pytis_crypto_encrypt_using_key', (TString, TString,), TString,
               body=pytis_crypto_encrypt_using_key,
               depends=())

sql_raw("""
create or replace function pytis_crypto_db_key (key_name_ text) returns text as $$
declare
  key text;
begin
  select public into strict key from pytis_crypto_db_keys where key_name=key_name_;
  return key;
exception
  when NO_DATA_FOUND then return null;
end;
$$ language plpgsql stable security definer;
""",
        name='pytis_crypto_db_key',
        depends=('e_pytis_crypto_keys', 'pytis_basic_crypto_functions', 'pytis_crypto_db_keys',))

sql_raw("""
create or replace function pytis_crypto_decrypt_db_password (password_ text, key_name_ text) returns text as $$
declare
  key text;
begin
  select private into strict key from pytis_crypto_db_keys where key_name=key_name_;
  return pytis_crypto_decrypt_using_key(key, password_);
exception
  when NO_DATA_FOUND then return password_;
end;
$$ language plpgsql stable;
""",
        name='pytis_crypto_decrypt_db_password',
        depends=('e_pytis_crypto_keys', 'pytis_basic_crypto_functions', 'pytis_crypto_db_keys',))

sql_raw("""
create or replace function pytis_crypto_create_db_key (key_name_ text, bits int) returns void as $$
  delete from pytis_crypto_db_keys where key_name=$1;
  insert into pytis_crypto_db_keys (select $1, * from pytis_crypto_generate_key($2));
$$ language sql;
""",
        name='pytis_crypto_create_db_key',
        depends=('e_pytis_crypto_keys', 'pytis_basic_crypto_functions', 'pytis_crypto_db_keys',))

sql_raw("""
create or replace function pytis_crypto_unlock_passwords (user_ text, password_ text) returns setof text as $$
declare
  plain_password text := pytis_crypto_decrypt_db_password(password_, 'pytis');
begin
  lock e_pytis_crypto_keys in exclusive mode;
  begin
    delete from t_pytis_passwords;
  exception
    when undefined_table then
      create temp table t_pytis_passwords (name text, password text);
  end;
  insert into t_pytis_passwords
         (select name, pytis_crypto_extract_key(key, plain_password)
                 from e_pytis_crypto_keys
                 where username=user_ and pytis_crypto_extract_key(key, plain_password) is not null);
  return query select name from t_pytis_passwords;
end;
$$ language plpgsql;
""",
        name='pytis_crypto_unlock_passwords',
        depends=('e_pytis_crypto_keys', 'pytis_basic_crypto_functions', 'pytis_crypto_db_keys',
                 'pytis_crypto_decrypt_db_password',))

sql_raw("""
create or replace function pytis_crypto_unlock_current_user_passwords (password_ text) returns setof text as $$
select * from pytis_crypto_unlock_passwords(current_user, $1);
$$ language sql;

-- create function pytis_crypto_user_contact (username text) returns pytis_crypto_t_user_contact as ...
""",
        name='pytis_crypto_unlock_current_user_passwords',
        depends=('e_pytis_crypto_keys', 'pytis_basic_crypto_functions', 'pytis_crypto_db_keys',
                 'pytis_crypto_unlock_passwords',))
