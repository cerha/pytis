# -*- coding: utf-8

from __future__ import unicode_literals

import pytis.data
import pytis.data.gensqlalchemy as sql
from pytis.dbdefs.db_pytis_base import pytis_schemas

class PytisCryptPassword(sql.SQLPlFunction):
    name = 'pytis_crypt_password'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('name_', pytis.data.String()),
    )
    result_type = pytis.data.String()
    multirow = False
    stability = 'STABLE'
    access_rights = ()

    def body(self):
        return """
declare
  psw text;
begin
  select password into strict psw from t_pytis_passwords where name=name_;
  return psw;
end;
        """

class PytisEncryptText(sql.SQLPlFunction):
    name = 'pytis_encrypt_text'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('data', pytis.data.String()),
        sql.Column('name', pytis.data.String()),
    )
    result_type = pytis.data.Binary()
    multirow = False
    access_rights = ()
    depends = (PytisCryptPassword,)

    def body(self):
        return """
begin
  return pgp_sym_encrypt(coalesce(data, ''), pytis_crypt_password(name));
end;
        """

class PytisDecryptText(sql.SQLPlFunction):
    name = 'pytis_decrypt_text'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('data', pytis.data.Binary()),
        sql.Column('name', pytis.data.String()),
    )
    result_type = pytis.data.String()
    multirow = False
    access_rights = ()
    depends = (PytisCryptPassword,)

    def body(self):
        return """
declare
  result text;
begin
  if data is null then
    return null;
  end if;
  begin
    result := rtrim(pgp_sym_decrypt(data, pytis_crypt_password(name)));
  exception
    when OTHERS then
      -- pseudorandom value to allow testing with obfuscated data
      result := (array['*encrypted*', '**encrypted**', '***encrypted***'])[floor(random()*3)+1];
  end;
  return result;
end;
        """

class PytisEncryptInt(sql.SQLPlFunction):
    name = 'pytis_encrypt_int'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('data', pytis.data.Integer()),
        sql.Column('name', pytis.data.String()),
    )
    result_type = pytis.data.Binary()
    multirow = False
    access_rights = ()
    depends = (PytisCryptPassword,)

    def body(self):
        return """
begin
  return pgp_sym_encrypt(coalesce(data::text, ''), pytis_crypt_password(name));
end;
        """


class PytisDecryptInt(sql.SQLPlFunction):
    name = 'pytis_decrypt_int'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('data', pytis.data.Binary()),
        sql.Column('name', pytis.data.String()),
    )
    result_type = pytis.data.Integer()
    multirow = False
    access_rights = ()
    depends = (PytisCryptPassword,)

    def body(self):
        return """
declare
  decrypted text;
  result int;
begin
  if data is null then
    return null;
  end if;
  begin
    decrypted := pgp_sym_decrypt(data, pytis_crypt_password(name));
    if decrypted = '' then
      return null;
    end if;
    result := decrypted::int;
  exception
    when OTHERS then
      -- pseudorandom value to allow testing with obfuscated data
      result := length(data) - 150;
  end;
  return result;
end;
        """


class PytisEncryptFloat(sql.SQLPlFunction):
    name = 'pytis_encrypt_float'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('data', pytis.data.Float()),
        sql.Column('name', pytis.data.String()),
    )
    result_type = pytis.data.Binary()
    multirow = False
    access_rights = ()
    depends = (PytisCryptPassword,)

    def body(self):
        return """
begin
  return pgp_sym_encrypt(coalesce(data::text, ''), pytis_crypt_password(name));
end;
        """

# TODO: it is necessary to allow same names in order to define
# overloaded functions
class PytisEncryptNumeric(sql.SQLRaw):
    name = 'pytis_encrypt_float_overloaded'
    schemas = pytis_schemas.value(globals())

    @classmethod
    def sql(class_):
        return """
create or replace function pytis_encrypt_float(data numeric, name text) returns bytea as $$
begin
  return pgp_sym_encrypt(coalesce(data::text, ''), pytis_crypt_password(name));
end;
$$ language plpgsql;
"""


class PytisDecryptFloat(sql.SQLPlFunction):
    name = 'pytis_decrypt_float'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('data', pytis.data.Binary()),
        sql.Column('name', pytis.data.String()),
    )
    result_type = pytis.data.Float()
    multirow = False
    access_rights = ()
    depends = (PytisCryptPassword,)

    def body(self):
        return """
declare
  decrypted text;
  result float;
begin
  if data is null then
    return null;
  end if;
  begin
    decrypted := pgp_sym_decrypt(data, pytis_crypt_password(name));
    if decrypted = '' then
      return null;
    end if;
    result := decrypted::float;
  exception
    when OTHERS then
      -- pseudorandom value to allow testing with obfuscated data
      result := (length(data) - 150)/10.0;
  end;
  return result;
end;
        """


class PytisEncryptBinary(sql.SQLPlFunction):
    name = 'pytis_encrypt_binary'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('data', pytis.data.Binary()),
        sql.Column('name', pytis.data.String()),
    )
    result_type = pytis.data.Binary()
    multirow = False
    access_rights = ()
    depends = (PytisCryptPassword,)

    def body(self):
        return """
begin
  return pgp_sym_encrypt(encode(coalesce(data, ''::bytea), 'base64'), pytis_crypt_password(name));
end;
        """


class PytisDecryptBinary(sql.SQLPlFunction):
    name = 'pytis_decrypt_binary'
    schemas = pytis_schemas.value(globals())
    arguments = (
        sql.Column('data', pytis.data.Binary()),
        sql.Column('name', pytis.data.String()),
    )
    result_type = pytis.data.Binary()
    multirow = False
    access_rights = ()
    depends = (PytisCryptPassword,)

    def body(self):
        return """
declare
  result bytea;
begin
  if data is null then
    return null;
  end if;
  begin
    result := decode(pgp_sym_decrypt(data, pytis_crypt_password(name)), 'base64');
  exception
    when OTHERS then
      -- pseudorandom value to allow testing with obfuscated data
      result := 'abc'::bytea;
  end;
  return result;
end;
        """
