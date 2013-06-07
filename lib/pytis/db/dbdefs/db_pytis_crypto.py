# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
import dbdefs as db

class CPytisCryptoNames(db.Base_LogSQLTable):
    """Codebook of encryption areas defined in the application."""
    name = 'c_pytis_crypto_names'
    fields = (
              sql.PrimaryColumn('name', pytis.data.String(not_null=False)),
              sql.Column('description', pytis.data.String(not_null=False)),
             )
    inherits = (db.XChanges,)
    with_oids = True
    depends_on = ()
    access_rights = db.default_access_rights.value(globals())

class EPytisCryptoKeys(db.Base_LogSQLTable):
    """Table of encryption keys of users for defined encryption areas."""
    name = 'e_pytis_crypto_keys'
    fields = (
              sql.PrimaryColumn('key_id', pytis.data.Serial()),
              sql.Column('name', pytis.data.String(not_null=True), references=sql.gA('c_pytis_crypto_names', onupdate='CASCADE')),
              sql.Column('username', pytis.data.String(not_null=True), doc="Arbitrary user identifier."),
              sql.Column('key', pytis.data.Binary(not_null=True)),
              sql.Column('fresh', pytis.data.Boolean(not_null=True), doc="Flag indicating the key is encrypted by a non-login password. ", default=False),
             )
    inherits = (db.XChanges,)
    with_oids = True
    unique = (('name', 'username',),)
    depends_on = (CPytisCryptoNames,)
    access_rights = db.default_access_rights.value(globals())

class EvPytisUserCryptoKeys(sql.SQLView):
    name = 'ev_pytis_user_crypto_keys'
    @classmethod
    def query(cls):
        keys = sql.t.EPytisCryptoKeys.alias('keys')
        return sqlalchemy.select(
            cls._exclude(keys, 'username', 'key'),
            from_obj=[keys],
            whereclause='username=current_user'
            )

    depends_on = (EPytisCryptoKeys,)
    access_rights = db.default_access_rights.value(globals())

class PytisCryptoKeyFunctions(sql.SQLRaw):
    name = 'pytis_crypto_key_functions'
    @classmethod
    def sql(class_):
        return """
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
"""
    depends_on = (EPytisCryptoKeys, db.PytisBasicCryptoFunctions,)

class PytisCryptoTUserContact(sql.SQLType):
    name = 'pytis_crypto_t_user_contact'
    fields = (
              sql.Column('email', pytis.data.String(not_null=False)),
              sql.Column('gpg_key', pytis.data.String(not_null=False)),
             )
    depends_on = ()
    access_rights = ()

class PytisCryptoDbKeys(sql.SQLTable):
    """
    Table of asymetric encryption keys.
    It is currently used to encrypt user passwords passed to some database functions.
    Use select pytis_crypto_create_db_key('pytis', 1024) to create a key for that purpose.
    """
    name = 'pytis_crypto_db_keys'
    fields = (
              sql.PrimaryColumn('key_name', pytis.data.String(not_null=False)),
              sql.Column('public', pytis.data.String(not_null=False)),
              sql.Column('private', pytis.data.String(not_null=False)),
             )
    with_oids = True
    depends_on = ()
    access_rights = ()

class PytisCryptoTKeyPair(sql.SQLType):
    name = 'pytis_crypto_t_key_pair'
    fields = (
              sql.Column('public', pytis.data.String(not_null=False)),
              sql.Column('private', pytis.data.String(not_null=False)),
             )
    depends_on = ()
    access_rights = ()

class PytisCryptoGenerateKey(db.Base_PyFunction):
    name = 'pytis_crypto_generate_key'
    arguments = (sql.Column('', pytis.data.Integer()),)
    result_type = PytisCryptoTKeyPair
    multirow = False
    stability = 'VOLATILE'
    depends_on = (PytisCryptoTKeyPair,)
    access_rights = ()

    @staticmethod
    def pytis_crypto_generate_key(bits):
        bits = args[0]
        import Crypto.PublicKey.RSA
        rsa = Crypto.PublicKey.RSA.generate(bits)
        public = rsa.publickey().exportKey()
        private = rsa.exportKey()
        return [public, private]



class PytisCryptoDecryptUsingKey(db.Base_PyFunction):
    name = 'pytis_crypto_decrypt_using_key'
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),)
    result_type = pytis.data.String()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def pytis_crypto_decrypt_using_key(private, encrypted):
        private, encrypted = args
        import Crypto.PublicKey.RSA, base64
        rsa = Crypto.PublicKey.RSA.importKey(private)
        return rsa.decrypt(base64.decodestring(encrypted))



class PytisCryptoEncryptUsingKey(db.Base_PyFunction):
    name = 'pytis_crypto_encrypt_using_key'
    arguments = (sql.Column('', pytis.data.String()),
                 sql.Column('', pytis.data.String()),)
    result_type = pytis.data.String()
    multirow = False
    stability = 'VOLATILE'
    depends_on = ()
    access_rights = ()

    @staticmethod
    def pytis_crypto_encrypt_using_key(public, text):
        public, text = args
        import Crypto.PublicKey.RSA, base64
        rsa = Crypto.PublicKey.RSA.importKey(public)
        encrypted = rsa.encrypt(text, None)[0]
        return base64.encodestring(encrypted)



class PytisCryptoDbKey(sql.SQLRaw):
    name = 'pytis_crypto_db_key'
    @classmethod
    def sql(class_):
        return """
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
"""
    depends_on = (EPytisCryptoKeys, db.PytisBasicCryptoFunctions, PytisCryptoDbKeys,)

class PytisCryptoDecryptDbPassword(sql.SQLRaw):
    name = 'pytis_crypto_decrypt_db_password'
    @classmethod
    def sql(class_):
        return """
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
"""
    depends_on = (EPytisCryptoKeys, db.PytisBasicCryptoFunctions, PytisCryptoDbKeys,)

class PytisCryptoCreateDbKey(sql.SQLRaw):
    name = 'pytis_crypto_create_db_key'
    @classmethod
    def sql(class_):
        return """
create or replace function pytis_crypto_create_db_key (key_name_ text, bits int) returns void as $$
  delete from pytis_crypto_db_keys where key_name=$1;
  insert into pytis_crypto_db_keys (select $1, * from pytis_crypto_generate_key($2));
$$ language sql;
"""
    depends_on = (EPytisCryptoKeys, db.PytisBasicCryptoFunctions, PytisCryptoDbKeys, PytisCryptoGenerateKey,)

class PytisCryptoUnlockPasswords(sql.SQLRaw):
    name = 'pytis_crypto_unlock_passwords'
    @classmethod
    def sql(class_):
        return """
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
"""
    depends_on = (EPytisCryptoKeys, db.PytisBasicCryptoFunctions, PytisCryptoDbKeys, PytisCryptoDecryptDbPassword,)

class PytisCryptoUnlockCurrentUserPasswords(sql.SQLRaw):
    name = 'pytis_crypto_unlock_current_user_passwords'
    @classmethod
    def sql(class_):
        return """
create or replace function pytis_crypto_unlock_current_user_passwords (password_ text) returns setof text as $$
select * from pytis_crypto_unlock_passwords(current_user, $1);
$$ language sql;

-- create function pytis_crypto_user_contact (username text) returns pytis_crypto_t_user_contact as ...
"""
    depends_on = (EPytisCryptoKeys, db.PytisBasicCryptoFunctions, PytisCryptoDbKeys, PytisCryptoUnlockPasswords,)

