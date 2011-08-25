# -*- coding: utf-8 -*-

# Copyright (C) 2011 Brailcom, o.p.s.
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
create or replace function pytis_crypt_password(name text) returns text as $$
declare
  psw text;
begin
  select password into strict psw from t_pytis_passwords where name=name;
  return psw;
end;
$$ language plpgsql stable;

create or replace function pytis_crypt_pad_text(string text) returns text as $$
declare
  static_length int := 100;
  random_length int := 100;
  padded text := string;
begin
  if length(padded) < static_length then
    padded := rpad(padded, static_length);
  end if;
  return rpad(padded, static_length + (random_length*random())::int);
end;
$$ language plpgsql;

create or replace function pytis_encrypt_text(data text, name text) returns bytea as $$
begin
  return pgp_sym_encrypt(pytis_crypt_pad_text(data), pytis_crypt_password(name));
end;
$$ language plpgsql;

create or replace function pytis_decrypt_text(data bytea, name text) returns text as $$
begin
  return rtrim(pgp_sym_decrypt(data, pytis_crypt_password(name)));
end;
$$ language plpgsql;

create or replace function pytis_encrypt_int(data int, name text) returns bytea as $$
begin
  return pgp_sym_encrypt(pytis_crypt_pad_text(data::text), pytis_crypt_password(name));
end;
$$ language plpgsql;

create or replace function pytis_decrypt_int(data bytea, name text) returns int as $$
declare
  result int;
begin
  begin
    result := pgp_sym_decrypt(data, pytis_crypt_password(name))::int;
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
  return pgp_sym_encrypt(pytis_crypt_pad_text(data::text), pytis_crypt_password(name));
end;
$$ language plpgsql;
create or replace function pytis_encrypt_float(data numeric, name text) returns bytea as $$
begin
  return pgp_sym_encrypt(pytis_crypt_pad_text(data::text), pytis_crypt_password(name));
end;
$$ language plpgsql;

create or replace function pytis_decrypt_float(data bytea, name text) returns float as $$
declare
  result float;
begin
  begin
    result := pgp_sym_decrypt(data, pytis_crypt_password(name))::float;
  exception
    when OTHERS then
      -- pseudorandom value to allow testing with obfuscated data
      result := (length(data) - 150)/10.0;
  end;
  return result;
end;
$$ language plpgsql;
""",
        name='pytis_crypto_functions')
