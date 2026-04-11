# -*- coding: utf-8 -*-

# Copyright (C) 2025 Tomáš Cerha <t.cerha@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

"""Pushed handler code for the JSON-over-TCP pytis2go service.

This module is NOT imported by the Pytis runtime on the server.  It is read as
a text file and pushed to the pytis2go client via the 'push_code' action,
where it is exec'd inside a namespace that provides:

  register(name, func)  -- add/override a request handler;
                           func(svc, request) -> result
  os, sys, base64, io, tempfile, hashlib, json  -- standard library modules

The handlers registered here augment (and may override) the built-in handlers
defined in protocol.py:

  session_password  -- return the X2Go session password stored by pytis2go
  open_file         -- adds GPG encrypt/decrypt support on top of the base handler
  open_selected_file -- adds GPG encrypt support
  make_selected_file -- adds GPG decrypt support
  make_temporary_file -- adds GPG decrypt support

GPG support requires the 'gnupg' Python package (python-gnupg) on the client.
If it is not installed, the GPG parameters are silently ignored and encryption
/ decryption is skipped.

"""

# ---------------------------------------------------------------------------
# session_password
# ---------------------------------------------------------------------------

def _session_password(svc, request):
    """Return the X2Go session password if the service recorded one."""
    # ServiceServer stores the session password in the ui_backend (pytis2go
    # sets it on _session_password attribute before pushing this code).
    return getattr(svc._ui_backend, '_session_password', None)

register('session_password', _session_password)

# ---------------------------------------------------------------------------
# GPG helpers
# ---------------------------------------------------------------------------

def _get_gpg():
    """Return a configured gnupg.GPG instance or None if gnupg is not available."""
    try:
        import gnupg
        return gnupg.GPG(options=['--trust-model', 'always'])
    except ImportError:
        return None


def _select_encryption_keys(gpg, keys):
    """Return a list of fingerprints for the given key specs."""
    fingerprints = []
    for key in keys:
        # key may be a fingerprint, a file path containing a key, or a key block
        if os.path.isfile(key):
            with open(key, 'rb') as f:
                result = gpg.import_keys(f.read())
        else:
            result = gpg.import_keys(key)
        fingerprints.extend(result.fingerprints)
    if not fingerprints:
        # fall back: use all available secret keys
        available = gpg.list_keys(True)
        fingerprints = [k['fingerprint'] for k in available]
    if not fingerprints:
        raise Exception("No encryption key found for GPG operation")
    return fingerprints


def _make_encrypt_func(gpg, keys):
    """Return a bytes->bytes function that GPG-encrypts its input."""
    def encrypt(data):
        selected = _select_encryption_keys(gpg, keys)
        result = gpg.encrypt(data, selected)
        if not result.ok:
            raise Exception("GPG encryption failed: " + str(result.stderr))
        return result.data
    return encrypt


def _make_decrypt_func(gpg, passphrase=None):
    """Return a bytes->bytes function that GPG-decrypts its input."""
    def decrypt(data):
        result = gpg.decrypt(data, passphrase=passphrase)
        if not result.ok:
            raise Exception("GPG decryption failed: " + str(result.stderr))
        return result.data
    return decrypt


# ---------------------------------------------------------------------------
# open_file — replaces base handler, adds GPG encrypt/decrypt
# ---------------------------------------------------------------------------

def _open_file(svc, request):
    path = request.get('path') or request.get('filename', '')
    mode = request.get('mode', 'rb')
    encoding = request.get('encoding')
    encrypt_keys = request.get('encrypt')   # list of key specs or None
    decrypt = request.get('decrypt', False)

    # No GPG involvement — open the file directly.
    # (Do not delegate to the base handler because the base handler expects
    # key 'path' but remote.py sends 'filename'; normalizing here avoids that.)
    if not encrypt_keys and not decrypt:
        kwargs = {'encoding': encoding} if encoding and 'b' not in mode else {}
        f = open(path, mode, **kwargs)
        return {'handle': svc.alloc_handle(path, f), 'path': path}

    gpg = _get_gpg()

    # For read modes with encryption: read file, encrypt in memory, expose via BytesIO
    if 'r' in mode and encrypt_keys and gpg:
        raw = open(path, 'rb').read()
        encrypted = _make_encrypt_func(gpg, encrypt_keys)(raw)
        f = io.BytesIO(encrypted)
        hid = svc.alloc_handle(path, f)
        return {'handle': hid, 'path': path}

    # For write modes with decryption: buffer writes, decrypt and write on close
    if 'w' in mode and decrypt and gpg:
        buf = io.BytesIO()
        # We need a wrapper that decrypts on close
        class DecryptOnClose:
            def __init__(self, target_path, buf, gpg, encoding):
                self._buf = buf
                self._path = target_path
                self._gpg = gpg
                self._encoding = encoding
                self.mode = mode
            def read(self, size=-1):
                return self._buf.read() if size < 0 else self._buf.read(size)
            def readline(self):
                return self._buf.readline()
            def readlines(self):
                return self._buf.readlines()
            def write(self, data):
                self._buf.write(data)
            def seek(self, pos, whence=0):
                return self._buf.seek(pos, whence)
            def flush(self):
                pass
            def fileno(self):
                raise io.UnsupportedOperation("fileno")
            def close(self):
                encrypted = self._buf.getvalue()
                decrypted = _make_decrypt_func(self._gpg)(encrypted)
                open_kwargs = {}
                if self._encoding:
                    decrypted = decrypted.decode(self._encoding)
                    open_kwargs['encoding'] = self._encoding
                # Determine the actual file open mode (strip 'b' if encoding)
                fmode = 'w' if self._encoding else 'wb'
                with open(self._path, fmode, **open_kwargs) as out:
                    out.write(decrypted)
        wrapper = DecryptOnClose(path, buf, gpg, encoding)
        hid = svc.alloc_handle(path, wrapper)
        return {'handle': hid, 'path': path}

    # GPG requested but unavailable — fall back to plain file
    kwargs = {'encoding': encoding} if encoding and 'b' not in mode else {}
    f = open(path, mode, **kwargs)
    return {'handle': svc.alloc_handle(path, f), 'path': path}

register('open_file', _open_file)


# ---------------------------------------------------------------------------
# open_selected_file — adds GPG encrypt support
# ---------------------------------------------------------------------------

def _open_selected_file(svc, request):
    encrypt_keys = request.get('encrypt')
    if not encrypt_keys:
        return svc._BASE_HANDLERS['open_selected_file'](svc, request)

    # Let the UI pick the file (reuse base handler's path)
    plain_result = svc._BASE_HANDLERS['open_selected_file'](svc, {
        k: v for k, v in request.items() if k != 'encrypt'
    })
    if plain_result is None:
        return None

    gpg = _get_gpg()
    if gpg is None:
        return plain_result

    path = plain_result['path']
    raw = open(path, 'rb').read()
    encrypted = _make_encrypt_func(gpg, encrypt_keys)(raw)
    f = io.BytesIO(encrypted)
    hid = svc.alloc_handle(path, f)
    return {'handle': hid, 'path': path}

register('open_selected_file', _open_selected_file)


# ---------------------------------------------------------------------------
# make_selected_file — adds GPG decrypt support
# ---------------------------------------------------------------------------

def _make_selected_file(svc, request):
    decrypt = request.get('decrypt', False)
    if not decrypt:
        return svc._BASE_HANDLERS['make_selected_file'](svc, request)

    plain_result = svc._BASE_HANDLERS['make_selected_file'](svc, {
        k: v for k, v in request.items() if k != 'decrypt'
    })
    if plain_result is None:
        return None

    gpg = _get_gpg()
    if gpg is None:
        return plain_result

    path = plain_result['path']
    encoding = request.get('encoding')
    mode = request.get('mode', 'wb')

    import io as _io
    buf = _io.BytesIO()

    class DecryptOnClose:
        def __init__(self):
            self.mode = mode
        def read(self, size=-1):
            return buf.read() if size < 0 else buf.read(size)
        def readline(self):
            return buf.readline()
        def readlines(self):
            return buf.readlines()
        def write(self, data):
            buf.write(data)
        def seek(self, pos, whence=0):
            return buf.seek(pos, whence)
        def flush(self):
            pass
        def fileno(self):
            raise _io.UnsupportedOperation("fileno")
        def close(self):
            encrypted = buf.getvalue()
            if encrypted:
                decrypted = _make_decrypt_func(gpg)(encrypted)
                fmode = 'w' if encoding else 'wb'
                open_kwargs = {'encoding': encoding} if encoding else {}
                if encoding:
                    decrypted = decrypted.decode(encoding)
                with open(path, fmode, **open_kwargs) as out:
                    out.write(decrypted)

    hid = svc.alloc_handle(path, DecryptOnClose())
    return {'handle': hid, 'path': path}

register('make_selected_file', _make_selected_file)


# ---------------------------------------------------------------------------
# make_temporary_file — adds GPG decrypt support
# ---------------------------------------------------------------------------

def _make_temporary_file(svc, request):
    decrypt = request.get('decrypt', False)
    if not decrypt:
        return svc._BASE_HANDLERS['make_temporary_file'](svc, request)

    gpg = _get_gpg()
    if gpg is None:
        return svc._BASE_HANDLERS['make_temporary_file'](svc, request)

    suffix = request.get('suffix', '')
    encoding = request.get('encoding')
    mode = request.get('mode', 'wb')

    # Create a real temp file that will hold the decrypted content
    fd, path = tempfile.mkstemp(suffix=suffix)
    os.close(fd)

    import io as _io
    buf = _io.BytesIO()

    class DecryptOnClose:
        def __init__(self):
            self.mode = mode
        def read(self, size=-1):
            return buf.read() if size < 0 else buf.read(size)
        def readline(self):
            return buf.readline()
        def readlines(self):
            return buf.readlines()
        def write(self, data):
            buf.write(data)
        def seek(self, pos, whence=0):
            return buf.seek(pos, whence)
        def flush(self):
            pass
        def fileno(self):
            raise _io.UnsupportedOperation("fileno")
        def close(self):
            encrypted = buf.getvalue()
            if encrypted:
                decrypted = _make_decrypt_func(gpg)(encrypted)
                fmode = 'w' if encoding else 'wb'
                open_kwargs = {'encoding': encoding} if encoding else {}
                if encoding:
                    decrypted = decrypted.decode(encoding)
                with open(path, fmode, **open_kwargs) as out:
                    out.write(decrypted)

    hid = svc.alloc_handle(path, DecryptOnClose())
    return {'handle': hid, 'path': path}

register('make_temporary_file', _make_temporary_file)
