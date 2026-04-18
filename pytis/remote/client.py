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

"""Client connector for the JSON-over-TCP pytis2go service.

This module replaces the RPyC-based `Connector` in remote.py.  It speaks the
protocol defined in pytis2go/service/protocol.py.

**Authentication** (raw bytes):

1. Server sends 64 random hex bytes (server challenge).
2. Client sends 64-byte client challenge + SHA-256 hex of
   XOR(password, client_challenge).
3. Server sends SHA-256 hex of XOR(password, server_challenge).

**JSON framing** (after auth): each message is a newline-terminated JSON
object.  Request: {"id": N, "action": "NAME", ...params...}.  Response:
{"id": N, "result": VALUE} on success, or {"id": N, "error": "MSG",
"traceback": "..."} on failure.

`ServiceClient` multiplexes concurrent requests over a single connection using
per-request ID queues.  `FileProxy` provides a file-like interface backed by a
remote file handle ID.

"""

import base64
import hashlib
import json
import os
import queue
import random
import socket
import threading


_CHALLENGE_LEN = 64


class AuthError(Exception):
    pass


class RemoteError(Exception):
    pass


# ---------------------------------------------------------------------------
# Authentication (client side only — mirrors PasswordAuthenticator in protocol.py)
# ---------------------------------------------------------------------------

class _ClientAuth:
    def __init__(self, password):
        assert len(password) == _CHALLENGE_LEN, repr(password)
        self._password = password

    def _challenge(self):
        chars = b'0123456789abcdef'
        r = random.SystemRandom()
        # r.choice(bytes) returns int in Python 3 but a 1-char str in Python 2.
        c = r.choice(chars)
        to_int = (lambda x: x) if isinstance(c, int) else ord
        return bytes(bytearray([to_int(r.choice(chars)) for _ in range(_CHALLENGE_LEN)]))

    def _hash(self, challenge):
        if isinstance(challenge, (bytes, bytearray)):
            challenge = challenge.decode('ascii')
        # bytes(generator) works in Python 3 but gives repr in Python 2; use bytearray.
        token = bytes(bytearray([ord(p) ^ ord(c) for p, c in zip(self._password, challenge)]))
        return hashlib.sha256(token).hexdigest().encode('ascii')

    def authenticate(self, sock):
        """Perform client-side mutual auth against a connected socket.

        Raises `AuthError` on failure.

        """
        server_challenge = _recv_exactly(sock, _CHALLENGE_LEN)
        client_challenge = self._challenge()
        sock.sendall(client_challenge + self._hash(client_challenge))
        server_hash = _recv_exactly(sock, _CHALLENGE_LEN)
        if server_hash != self._hash(server_challenge):
            raise AuthError("Invalid server password hash")


def _recv_exactly(sock, n):
    data = b''
    while len(data) < n:
        chunk = sock.recv(n - len(data))
        if not chunk:
            raise AuthError("Connection closed during auth")
        data += chunk
    return data


# ---------------------------------------------------------------------------
# ServiceClient
# ---------------------------------------------------------------------------

class ServiceClient:
    """Thread-safe JSON-over-TCP client for the pytis2go service.

    One instance covers one logical session (connection + optional reconnection).
    Multiple threads may call `request` concurrently; each gets its own
    response queue keyed by the auto-incremented request ID.

    """

    def __init__(self, password):
        self._auth = _ClientAuth(password)
        self._sock = None
        self._rfile = None
        self._lock = threading.Lock()        # guards _sock / send path
        self._pending = {}                   # id -> Queue
        self._pending_lock = threading.Lock()
        self._next_id = 1
        self._reader_thread = None

    # ------------------------------------------------------------------
    # Connection management
    # ------------------------------------------------------------------

    def connect(self, host, port):
        """Open a connection to the service and authenticate.

        May be called again after a disconnect to reconnect.
        Raises `AuthError` or `OSError` on failure.

        """
        sock = socket.create_connection((host, port), timeout=10)
        sock.settimeout(None)
        self._auth.authenticate(sock)
        with self._lock:
            self._sock = sock
            self._rfile = sock.makefile('rb')
            self._pending.clear()
            t = threading.Thread(target=self._reader_loop)
            t.daemon = True
            self._reader_thread = t
        t.start()

    def disconnect(self):
        """Close the connection."""
        with self._lock:
            if self._sock is not None:
                try:
                    self._sock.close()
                except OSError:
                    pass
                self._sock = None
        self._wake_pending(RemoteError("Disconnected"))

    def is_connected(self):
        return self._sock is not None

    # ------------------------------------------------------------------
    # Request/response
    # ------------------------------------------------------------------

    def request(self, rpc_action, **params):
        """Send a request and block until the response arrives.

        Returns the 'result' value on success.
        Raises `RemoteError` (with error message) on failure.

        """
        with self._lock:
            if self._sock is None:
                raise RemoteError("Not connected")
            req_id = self._next_id
            self._next_id += 1
            q = queue.Queue()
            with self._pending_lock:
                self._pending[req_id] = q
            msg = json.dumps(dict(id=req_id, action=rpc_action, **params))
            try:
                self._sock.sendall((msg + '\n').encode('utf-8'))
            except OSError as e:
                with self._pending_lock:
                    self._pending.pop(req_id, None)
                raise RemoteError(str(e))

        response = q.get()
        if isinstance(response, Exception):
            raise response
        if 'error' in response:
            raise RemoteError(response['error'])
        return response['result']

    # ------------------------------------------------------------------
    # File handle helpers
    # ------------------------------------------------------------------

    def open_file(self, path, mode, encoding=None):
        """Open a remote file and return a `FileProxy`."""
        result = self.request('open_file', path=path, mode=mode, encoding=encoding)
        return FileProxy(self, result['handle'], result['path'], mode)

    def open_selected_file(self, directory=None, patterns=(), pattern=None, encrypt=None):
        """Show a file-open dialog on the client; return `FileProxy` or `None`."""
        result = self.request('open_selected_file', directory=directory,
                              patterns=list(patterns), pattern=pattern, encrypt=encrypt)
        if result is None:
            return None
        return FileProxy(self, result['handle'], result['path'], 'rb')

    def make_selected_file(self, directory=None, filename=None, patterns=(), pattern=None,
                           encoding=None, mode='wb'):
        """Show a file-save dialog on the client; return `FileProxy` or `None`."""
        result = self.request('make_selected_file', directory=directory, filename=filename,
                              patterns=list(patterns), pattern=pattern,
                              encoding=encoding, mode=mode)
        if result is None:
            return None
        return FileProxy(self, result['handle'], result['path'], mode)

    def make_temporary_file(self, suffix='', encoding=None, mode='wb'):
        """Create a temporary file on the client; return `FileProxy`."""
        result = self.request('make_temporary_file', suffix=suffix, encoding=encoding, mode=mode)
        return FileProxy(self, result['handle'], result['path'], mode)

    # ------------------------------------------------------------------
    # Internal
    # ------------------------------------------------------------------

    def _reader_loop(self):
        """Background thread: read responses and dispatch to waiting queues."""
        try:
            while True:
                line = self._rfile.readline()
                if not line:
                    break
                try:
                    response = json.loads(line.decode('utf-8'))
                except (ValueError, UnicodeDecodeError):
                    continue
                req_id = response.get('id')
                if req_id is not None:
                    with self._pending_lock:
                        q = self._pending.pop(req_id, None)
                    if q is not None:
                        q.put(response)
        except OSError:
            pass
        finally:
            with self._lock:
                self._sock = None
            self._wake_pending(RemoteError("Connection lost"))

    def _wake_pending(self, exc):
        with self._pending_lock:
            for q in self._pending.values():
                q.put(exc)
            self._pending.clear()


# ---------------------------------------------------------------------------
# FileProxy — file-like object backed by a remote file handle ID
# ---------------------------------------------------------------------------

class FileProxy:
    """A file-like object whose I/O is forwarded to a remote handle on the client.

    Binary data is transparently base64-encoded in transit.

    """

    def __init__(self, client, handle_id, path, mode):
        self._client = client
        self._handle = handle_id
        self._path = path
        self._mode = mode
        self._closed = False

    @property
    def name(self):
        return self._client.request('file_name', handle=self._handle)

    @property
    def mode(self):
        return self._mode

    def read(self, size=-1):
        raw = self._client.request('file_read', handle=self._handle, size=size)
        if 'b' in self._mode and raw is not None:
            return base64.b64decode(raw)
        return raw

    def readline(self):
        raw = self._client.request('file_readline', handle=self._handle)
        if 'b' in self._mode and raw is not None:
            return base64.b64decode(raw)
        return raw

    def readlines(self):
        raws = self._client.request('file_readlines', handle=self._handle)
        if 'b' in self._mode:
            return [base64.b64decode(r) for r in raws]
        return raws

    def write(self, data):
        if isinstance(data, (bytes, bytearray)):
            payload = base64.b64encode(data).decode('ascii')
        else:
            payload = data
        self._client.request('file_write', handle=self._handle, data=payload)

    def seek(self, pos, whence=0):
        return self._client.request('file_seek', handle=self._handle, pos=pos, whence=whence)

    def flush(self):
        self._client.request('file_flush', handle=self._handle)

    def close(self):
        if not self._closed:
            self._closed = True
            try:
                self._client.request('file_close', handle=self._handle)
            except RemoteError:
                pass

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

    def __repr__(self):
        return '<FileProxy handle={} path={!r} mode={!r}>'.format(
            self._handle, self._path, self._mode)
