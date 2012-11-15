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

sql_raw("""
create or replace function pytis_crypt_password(name_ text) returns text as $$
declare
  psw text;
begin
  select password into strict psw from t_pytis_passwords where name=name_;
  return psw;
end;
$$ language plpgsql stable;

create or replace function pytis_encrypt_text(data text, name text) returns bytea as $$
begin
  return pgp_sym_encrypt(coalesce(data, ''), pytis_crypt_password(name));
end;
$$ language plpgsql;

create or replace function pytis_decrypt_text(data bytea, name text) returns text as $$
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
$$ language plpgsql;

create or replace function pytis_encrypt_int(data int, name text) returns bytea as $$
begin
  return pgp_sym_encrypt(coalesce(data::text, ''), pytis_crypt_password(name));
end;
$$ language plpgsql;

create or replace function pytis_decrypt_int(data bytea, name text) returns int as $$
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
$$ language plpgsql;

create or replace function pytis_encrypt_float(data float, name text) returns bytea as $$
begin
  return pgp_sym_encrypt(coalesce(data::text, ''), pytis_crypt_password(name));
end;
$$ language plpgsql;
create or replace function pytis_encrypt_float(data numeric, name text) returns bytea as $$
begin
  return pgp_sym_encrypt(coalesce(data::text, ''), pytis_crypt_password(name));
end;
$$ language plpgsql;

create or replace function pytis_decrypt_float(data bytea, name text) returns float as $$
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
$$ language plpgsql;

create or replace function pytis_encrypt_binary(data bytea, name text) returns bytea as $$
begin
  return pgp_sym_encrypt(encode(coalesce(data, ''::bytea), 'base64'), pytis_crypt_password(name));
end;
$$ language plpgsql;

create or replace function pytis_decrypt_binary(data bytea, name text) returns bytea as $$
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
$$ language plpgsql;
""",
        name='pytis_basic_crypto_functions')
