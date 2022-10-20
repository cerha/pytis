# -*- coding: utf-8

from __future__ import unicode_literals

import sqlalchemy
import pytis.data.gensqlalchemy as sql
import pytis.data
from pytis.dbdefs.db_pytis_base import (
    Base_LogSQLTable, Base_PyFunction,
    default_access_rights, pytis_schemas
)
from pytis.dbdefs.db_pytis_common import XChanges
from pytis.dbdefs.db_pytis_crypto_basic import PytisCryptPassword
import pytis.util

_ = pytis.util.translations('pytis-wx')

crypto_select_rights = sql.SQLFlexibleValue('app_crypto_select_rights',
                                            environment='GSQL_CRYPTO_SELECT_RIGHTS',
                                            default=(('select', 'pytis',),))


class CPytisCryptoNames(Base_LogSQLTable):
    """Codebook of encryption areas defined in the application."""
    name = 'c_pytis_crypto_names'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('name', pytis.data.String(not_null=False),
                                label=_("Šifrovací oblast")),
              sql.Column('description', pytis.data.String(not_null=False), label=_("Popis")),
              )
    inherits = (XChanges,)
    depends_on = ()
    access_rights = default_access_rights.value(globals())


class EPytisCryptoKeys(Base_LogSQLTable):
    """Table of encryption keys of users for defined encryption areas."""
    name = 'e_pytis_crypto_keys'
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('key_id', pytis.data.Serial()),
              sql.Column('name', pytis.data.String(not_null=True), label=_("Šifrovací oblast"),
                         references=sql.a(sql.r.CPytisCryptoNames.name, onupdate='CASCADE')),
              sql.Column('username', pytis.data.String(not_null=True), label=_("Uživatel"),
                         doc="Arbitrary user identifier."),
              sql.Column('key', pytis.data.Binary(not_null=True)),
              sql.Column('fresh', pytis.data.Boolean(not_null=True), label=_("Nový"),
                         doc="Flag indicating the key is encrypted by a non-login password. ",
                         default=False),
              )
    inherits = (XChanges,)
    unique = (('name', 'username',),)
    depends_on = (CPytisCryptoNames,)
    access_rights = default_access_rights.value(globals())


class EvPytisUserCryptoKeys(sql.SQLView):
    name = 'ev_pytis_user_crypto_keys'
    schemas = pytis_schemas.value(globals())

    @classmethod
    def query(cls):
        keys = sql.t.EPytisCryptoKeys.alias('keys')
        return sqlalchemy.select(
            cls._exclude(keys, 'username', 'key'),
            from_obj=[keys],
            whereclause=keys.c.username == sqlalchemy.text('current_user'),
        )

    depends_on = (EPytisCryptoKeys,)
    access_rights = default_access_rights.value(globals())


class PytisCryptoExtractKey(sql.SQLPlFunction):
    name = 'pytis_crypto_extract_key'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('encrypted', pytis.data.Binary()),
        sql.Column('psw', pytis.data.String()),
    )
    result_type = pytis.data.String()
    multirow = False
    stability = 'IMMUTABLE'
    access_rights = ()
    depends_on = (EPytisCryptoKeys,)

    def body(self):
        return """
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
        """

class PytisCryptoStoreKey(sql.SQLPlFunction):
    name = 'pytis_crypto_store_key'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('key', pytis.data.String()),
        sql.Column('psw', pytis.data.String()),
    )
    result_type = pytis.data.Binary()
    multirow = False
    stability = 'IMMUTABLE'
    access_rights = ()
    depends_on = (EPytisCryptoKeys,)

    def body(self):
        return """
begin
  return pgp_sym_encrypt('pytis:'||$1, $2);
end;
        """

class PytisCryptoInsertKey(sql.SQLPlFunction):
    name = 'pytis_crypto_insert_key'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('name_', pytis.data.String()),
        sql.Column('user_', pytis.data.String()),
        sql.Column('key_', pytis.data.String()),
        sql.Column('psw', pytis.data.String()),
    )
    result_type = pytis.data.Boolean()
    multirow = False
    stability = 'VOLATILE'
    access_rights = ()
    depends_on = (EPytisCryptoKeys, PytisCryptoStoreKey)

    def body(self):
        return """
begin
  lock e_pytis_crypto_keys in exclusive mode;
  if (select count(*) from e_pytis_crypto_keys where name=name_ and username=user_) > 0 then
    return False;
  end if;
  insert into e_pytis_crypto_keys (name, username, key)
        values (name_, user_, pytis_crypto_store_key(key_, psw));
  return True;
end;
        """

class PytisCryptoChangePassword(sql.SQLPlFunction):
    name = 'pytis_crypto_change_password'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('id_', pytis.data.Integer()),
        sql.Column('old_psw', pytis.data.String()),
        sql.Column('new_psw', pytis.data.String()),
    )
    result_type = pytis.data.Boolean()
    multirow = False
    stability = 'VOLATILE'
    access_rights = ()
    security_definer = True
    depends_on = (EPytisCryptoKeys, PytisCryptoExtractKey)

    def body(self):
        return """
declare
  key_ text;
  plain_old_psw text := pytis_crypto_decrypt_db_password(old_psw, 'pytis');
  plain_new_psw text := pytis_crypto_decrypt_db_password(new_psw, 'pytis');
begin
  lock e_pytis_crypto_keys in exclusive mode;
  begin
    select pytis_crypto_extract_key(key, plain_old_psw) into key_ from e_pytis_crypto_keys
        where key_id=id_;
  exception
    when OTHERS then
      key_ := null;
  end;
  if key_ is null then
    return False;
  end if;
  update e_pytis_crypto_keys set key=pytis_crypto_store_key(key_, plain_new_psw), fresh=False
        where key_id=id_;
  return True;
end;
        """

class PytisCryptoCopyKey(sql.SQLPlFunction):
    name = 'pytis_crypto_copy_key'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('name_', pytis.data.String()),
        sql.Column('from_user', pytis.data.String()),
        sql.Column('to_user', pytis.data.String()),
        sql.Column('from_psw', pytis.data.String()),
        sql.Column('to_psw', pytis.data.String()),
    )
    result_type = pytis.data.Boolean()
    multirow = False
    stability = 'VOLATILE'
    access_rights = ()
    depends_on = (EPytisCryptoKeys, PytisCryptoExtractKey, PytisCryptoStoreKey)

    def body(self):
        return """
declare
  key_ text;
begin
  lock e_pytis_crypto_keys in exclusive mode;
  begin
    select pytis_crypto_extract_key(key, from_psw) into key_ from e_pytis_crypto_keys
        where name=name_ and username=from_user;
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
        """


class PytisCryptoDeleteKey(sql.SQLPlFunction):
    name = 'pytis_crypto_delete_key'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('name_', pytis.data.String()),
        sql.Column('user_', pytis.data.String()),
        sql.Column('force', pytis.data.Boolean()),
    )
    result_type = pytis.data.Boolean()
    multirow = False
    stability = 'VOLATILE'
    access_rights = ()
    depends_on = (EPytisCryptoKeys,)

    def body(self):
        return """
begin
  lock e_pytis_crypto_keys in exclusive mode;
  if not force and (select count(*) from e_pytis_crypto_keys where name=name_) <= 1 then
    return False;
  end if;
  delete from e_pytis_crypto_keys where name=name_ and username=user_;
  return True;
end;
        """


class PytisCryptoTUserContact(sql.SQLType):
    name = 'pytis_crypto_t_user_contact'
    schemas = pytis_schemas.value(globals())
    fields = (sql.Column('email', pytis.data.String(not_null=False)),
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
    schemas = pytis_schemas.value(globals())
    fields = (sql.PrimaryColumn('key_name', pytis.data.String(not_null=False)),
              sql.Column('public', pytis.data.String(not_null=False)),
              sql.Column('private', pytis.data.String(not_null=False)),
              )
    depends_on = ()
    access_rights = crypto_select_rights.value(globals())


class PytisCryptoTKeyPair(sql.SQLType):
    name = 'pytis_crypto_t_key_pair'
    schemas = pytis_schemas.value(globals())
    fields = (sql.Column('public', pytis.data.String(not_null=False)),
              sql.Column('private', pytis.data.String(not_null=False)),
              )
    depends_on = ()
    access_rights = ()


class PytisCryptoGenerateKey(Base_PyFunction):
    name = 'pytis_crypto_generate_key'
    schemas = pytis_schemas.value(globals())
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
        if isinstance(private, bytes):
            return [public.decode('utf-8'), private.decode('utf-8')]
        else:
            return [public, private]


class PytisCryptoDecryptUsingKey(Base_PyFunction):
    name = 'pytis_crypto_decrypt_using_key'
    schemas = pytis_schemas.value(globals())
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
        import Crypto.PublicKey.RSA
        import base64
        private = private.encode('utf-8')
        encrypted = encrypted.encode('utf-8')
        rsa = Crypto.PublicKey.RSA.importKey(private)
        decrypted = rsa.decrypt(base64.decodestring(encrypted))
        if isinstance(decrypted, bytes):
            decrypted = decrypted.decode('utf-8')
        return decrypted


class PytisCryptoEncryptUsingKey(Base_PyFunction):
    name = 'pytis_crypto_encrypt_using_key'
    schemas = pytis_schemas.value(globals())
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
        import Crypto.PublicKey.RSA
        import base64
        public = public.encode('utf-8')
        text = text.encode('utf-8')
        rsa = Crypto.PublicKey.RSA.importKey(public)
        encrypted = rsa.encrypt(text, None)[0]
        encoded = base64.b64encode(encrypted)
        if isinstance(encoded, bytes):
            encoded = encoded.decode('utf-8')
        return encoded


class PytisCryptoDbKey(sql.SQLPlFunction):
    name = 'pytis_crypto_db_key'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('key_name_', pytis.data.String()),
    )
    result_type = pytis.data.String()
    multirow = False
    stability = 'STABLE'
    security_definer = True
    access_rights = ()
    depends_on = (EPytisCryptoKeys, PytisCryptoDbKeys,)

    def body(self):
        return """
declare
  key text;
begin
  select public into strict key from pytis_crypto_db_keys where key_name=key_name_;
  return key;
exception
  when NO_DATA_FOUND then return null;
end;
        """


class PytisCryptoDecryptDbPassword(sql.SQLPlFunction):
    name = 'pytis_crypto_decrypt_db_password'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('password_', pytis.data.String()),
        sql.Column('key_name_', pytis.data.String()),
    )
    result_type = pytis.data.String()
    multirow = False
    stability = 'STABLE'
    access_rights = ()
    depends_on = (EPytisCryptoKeys, PytisCryptoDbKeys, PytisCryptoDecryptUsingKey)

    def body(self):
        return """
declare
  key text;
begin
  select private into strict key from pytis_crypto_db_keys where key_name=key_name_;
  return pytis_crypto_decrypt_using_key(key, password_);
exception
  when NO_DATA_FOUND then return password_;
end;
        """


class PytisCryptoCreateDbKey(sql.SQLFunction):
    name = 'pytis_crypto_create_db_key'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('key_name_', pytis.data.String()),
        sql.Column('bits', pytis.data.Integer()),
    )
    result_type = None
    multirow = False
    access_rights = ()
    depends_on = (EPytisCryptoKeys, PytisCryptoDbKeys, PytisCryptoGenerateKey,)

    def body(self):
        return """
  delete from pytis_crypto_db_keys where key_name=$1;
  insert into pytis_crypto_db_keys (select $1, * from pytis_crypto_generate_key($2));
        """


class PytisCryptoUnlockPasswords(sql.SQLPlFunction):
    name = 'pytis_crypto_unlock_passwords'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('user_', pytis.data.String()),
        sql.Column('password_', pytis.data.String()),
    )
    result_type = pytis.data.String()
    multirow = True
    access_rights = ()
    depends_on = (EPytisCryptoKeys, PytisCryptoDbKeys, PytisCryptoDecryptDbPassword,)

    def body(self):
        return """
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
                 where username=user_ and
                       pytis_crypto_extract_key(key, plain_password) is not null);
  return query select name from t_pytis_passwords;
end;
        """


class PytisCryptoUnlockCurrentUserPasswords(sql.SQLFunction):
    name = 'pytis_crypto_unlock_current_user_passwords'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('password_', pytis.data.String()),
    )
    result_type = pytis.data.String()
    multirow = True
    access_rights = ()
    depends_on = (EPytisCryptoKeys, PytisCryptoDbKeys, PytisCryptoUnlockPasswords,)

    def body(self):
        return "select * from pytis_crypto_unlock_passwords(current_user, $1)"
