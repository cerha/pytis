#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Copyright (C) 2025-2026 Tomáš Cerha <t.cerha@gmail.com>
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
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Dump searchable data from a PostgreSQL database and search it efficiently.

The script has two subcommands:

- dump:
  Exports searchable (non-binary) columns from all tables into a stream:
  - One JSON metadata line per table (schema/table, dumped columns, key columns),
  - followed by TSV rows (tabs/newlines/CR in values are normalized to spaces).

  Default dumped columns:
  - key columns (primary key; else NOT NULL unique key; else ctid),
  - plus text-like columns (typcategory='S', incl. citext).

- search:
  Searches the dump for a list of terms and reports counts and locations
  (table, column, key, context).

The intended workflow is: dump once, search many times.  The dump format is
optimized for fast text searching, not for database restore.

"""

import contextlib
import functools
import os
import re
import sys
import json
import getpass
import argparse
import shutil
import subprocess
import tempfile

import psycopg2
import psycopg2.extras
import psycopg2.extensions


META_PREFIX = '# META = '
SYSTEM_SCHEMAS = set(('pg_catalog', 'information_schema', 'pg_toast'))


def quote_ident(connection, *parts: str) -> str:
    return '.'.join(psycopg2.extensions.quote_ident(ident, connection) for ident in parts)


def error(*messages):
    print(os.path.basename(sys.argv[0]), ' '.join(sys.argv[1:]), file=sys.stderr)
    print('ERROR:', "\n".join(messages), file=sys.stderr)


@contextlib.contextmanager
def dbconnection(args):
    password = args.password or os.getenv("PGPASSWORD")

    while True:
        try:
            with psycopg2.connect(
                dbname=args.dbname,
                user=args.user,
                host=args.host,
                port=args.port,
                password=password,
            ) as connection:
                connection.set_session(readonly=True, autocommit=True)
                yield connection
            break
        except psycopg2.OperationalError as e:
            msg = str(e).lower()
            if password is None and (
                "password authentication failed" in msg
                or "no password supplied" in msg
                or "authentication failed" in msg
                or getattr(e, "pgcode", None) in ("28P01", "28000")
            ):
                try:
                    password = getpass.getpass("Password: ")
                    continue
                except (EOFError, KeyboardInterrupt):
                    error("Password not provided.")
                    sys.exit(1)
            error(f"PostgreSQL: {e}")
            sys.exit(2)


@contextlib.contextmanager
def fopen(fname, mode='r', encoding='utf-8'):
    if fname == '-':
        fh = sys.stdout if 'w' in mode else sys.stdin
        yield fh
    else:
        fh = open(fname, mode, encoding=encoding)
        try:
            yield fh
        finally:
            fh.close()


def get_primary_key(connection, schema: str, table: str):
    """Return list of PK columns in order, or [] if no PK."""
    sql = """
        SELECT a.attname
        FROM pg_index i
        JOIN pg_class c ON c.oid = i.indrelid
        JOIN pg_namespace n ON n.oid = c.relnamespace
        JOIN unnest(i.indkey) WITH ORDINALITY AS k(attnum, ord) ON TRUE
        JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = k.attnum
        WHERE i.indisprimary AND n.nspname = %s AND c.relname = %s
        ORDER BY k.ord;
    """
    with connection.cursor() as cur:
        cur.execute(sql, (schema, table))
        return [r[0] for r in cur.fetchall()]


def get_best_unique_key(connection, schema: str, table: str):
    """Return list of NOT NULL unique key columns (smallest arity), or [] if none."""
    sql = """
        WITH uniq AS (
            SELECT
                i.indexrelid AS index_oid,
                i.indrelid   AS table_oid,
                i.indkey     AS indkey
            FROM pg_index i
            JOIN pg_class c ON c.oid = i.indrelid
            JOIN pg_namespace n ON n.oid = c.relnamespace
            WHERE i.indisunique
              AND NOT i.indisprimary
              AND n.nspname = %s
              AND c.relname = %s
        ),
        cols AS (
            SELECT
                u.index_oid,
                k.ord,
                a.attname,
                a.attnotnull
            FROM uniq u
            JOIN unnest(u.indkey) WITH ORDINALITY AS k(attnum, ord) ON TRUE
            JOIN pg_attribute a ON a.attrelid = u.table_oid AND a.attnum = k.attnum
            WHERE k.attnum > 0
        ),
        grouped AS (
            SELECT
                index_oid,
                array_agg(attname ORDER BY ord) AS colnames,
                bool_and(attnotnull)            AS all_not_null,
                count(*)                        AS ncols
            FROM cols
            GROUP BY index_oid
        )
        SELECT colnames
        FROM grouped
        WHERE all_not_null
        ORDER BY ncols, index_oid
        LIMIT 1;
    """
    with connection.cursor() as cur:
        cur.execute(sql, (schema, table))
        row = cur.fetchone()
        return list(row[0]) if row else []


def iter_tables(connection, include_schemas=None, exclude_schemas=None):
    include_schemas = set(include_schemas or ())
    exclude_schemas = set(exclude_schemas or ())
    if not include_schemas:
        exclude_schemas |= SYSTEM_SCHEMAS

    sql = """
        SELECT n.nspname AS schema_name,
               c.relname AS table_name
        FROM pg_catalog.pg_class c
        JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
        WHERE c.relkind = 'r'
          AND n.nspname NOT LIKE 'pg_temp_%'
          AND n.nspname NOT LIKE 'pg_toast_temp_%'
        ORDER BY n.nspname, c.relname;
    """
    with connection.cursor() as cur:
        cur.execute(sql)
        for schema_name, table_name in cur:
            if include_schemas and schema_name not in include_schemas:
                continue
            if schema_name in exclude_schemas:
                continue
            yield schema_name, table_name


def get_columns(connection, schema: str, table: str):
    """Return ordered columns with type info."""
    sql = """
        SELECT
            a.attname,
            t.typname,
            t.typcategory,
            bt.typname AS base_typname
        FROM pg_catalog.pg_attribute a
        JOIN pg_catalog.pg_class c ON c.oid = a.attrelid
        JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
        JOIN pg_catalog.pg_type t ON t.oid = a.atttypid
        LEFT JOIN pg_catalog.pg_type bt ON bt.oid = t.typbasetype
        WHERE n.nspname = %s
          AND c.relname = %s
          AND a.attnum > 0
          AND NOT a.attisdropped
        ORDER BY a.attnum;
    """
    cols = []
    with connection.cursor(cursor_factory=psycopg2.extras.DictCursor) as cur:
        cur.execute(sql, (schema, table))
        for row in cur.fetchall():
            cols.append({
                "name": row["attname"],
                "typname": row["typname"],
                "typcategory": row["typcategory"],
                "base_typname": row["base_typname"],
            })
    return cols


def sanitize(expr: str) -> str:
    # Replacements (done in SQL):
    # - ESC (chr(27)) -> ' '
    # - VT  (chr(11)) -> ' '
    # - FF  (chr(12)) -> ' '
    # - "   (chr(34)) -> '
    # - tabs/CR/LF -> ' ' (via regexp_replace)
    from_chars = 'chr(27) || chr(11) || chr(12) || chr(34)'
    to_chars = "'   '''"  # SQL literal: 3 spaces + single quote (4 chars total)
    return (
        'regexp_replace('
        f'translate({expr}::text, {from_chars}, {to_chars}), '
        "E'[\\t\\r\\n]+', ' ', 'g')"
    )


def dump(args):
    with dbconnection(args) as connection, fopen(args.output, 'w') as output:
        qi = functools.partial(quote_ident, connection)
        for schema, table in iter_tables(connection,
                                         include_schemas=args.include_schema,
                                         exclude_schemas=args.exclude_schema):
            key = (get_primary_key(connection, schema, table) or
                   get_best_unique_key(connection, schema, table) or
                   ['ctid'])
            columns = [c for c in get_columns(connection, schema, table)
                       if not (c['typname'] == 'bytea' or c.get('base_typname') == 'bytea')]
            if args.include_nontext:
                searchable_columns = [c['name'] for c in columns]
            else:
                searchable_columns = [c['name'] for c in columns
                                      if c['typcategory'] == 'S' or c['typname'] == 'citext']
            dump_columns = list(key) + [c for c in searchable_columns if c not in key]

            print(META_PREFIX + json.dumps({
                'schema': schema,
                'table': table,
                'key': key,
                'columns': dump_columns,
            }, ensure_ascii=False), file=output)
            copy = "COPY ({}) TO STDOUT WITH (FORMAT csv, DELIMITER E'\\t', NULL {})".format(
                "SELECT {} FROM {}".format(
                    ', '.join([sanitize(qi(c)) for c in dump_columns]),
                    qi(schema, table)
                ),
                psycopg2.extensions.adapt(args.null).getquoted().decode('utf-8'))
            with connection.cursor() as cur:
                cur.copy_expert(copy, output)
            print(file=output)


def load_patterns(path: str):
    with fopen(path, 'r', encoding='utf-8') as fh:
        patterns = []
        for line in fh:
            pattern = line.strip()
            if pattern and not pattern.startswith("#"):
                patterns.append(pattern)
        return patterns


def compile_patterns_regex(patterns, regex=False):
    # Named groups so we know which patterns matched.
    # Note: group names must be identifiers, so we use t0,t1,...
    parts = []
    group_to_pattern = {}
    for i, pattern in enumerate(patterns):
        g = f't{i}'
        expr = pattern if regex else re.escape(pattern)
        parts.append(f'(?P<{g}>{expr})')
        group_to_pattern[g] = pattern
    return re.compile('|'.join(parts), re.IGNORECASE), group_to_pattern


def format_key_value(v: str):
    if v and re.fullmatch(r'-?\d+', v):
        return v
    else:
        return f"'{v}'"


def format_key(keyidx, row):
    return ', '.join(k + '=' + format_key_value(row[i]) for i, k in keyidx)


def format_snippet(value: str, start: int, end: int, context: int):
    start = max(0, start - context)
    end = min(len(value), end + context)
    prefix = '…' if start > 0 else ''
    suffix = '…' if end < len(value) else ''
    return prefix + value[start:end] + suffix


@contextlib.contextmanager
def prefilter_input(input_path, patterns, prefilter='grep', regex=False):
    with tempfile.NamedTemporaryFile(mode='w', encoding='utf-8', delete=True) as pf:
        # Save patterns for grep -f (empty lines and comments dropped earlier in load_patterns()).
        pf.write('\n'.join(patterns) + '\n')
        pf.flush()

        # In fixed-string mode (-F) we cannot anchor to line start; '^' would be literal.
        # META_PREFIX + '{' makes accidental matches in TSV unlikely, and we still validate
        # meta lines in Python (startswith(META_PREFIX)) before json.loads().
        meta = META_PREFIX + '{'
        if regex:
            meta = '^' + re.escape(meta)

        if prefilter == 'grep':
            argv = ['grep', '-E' if regex else '-F', '-i', '-e', meta, '-f', pf.name]
        else:
            argv = ['rg', '-i', '-e', meta, '-f', pf.name]
            if not regex:
                argv.insert(1, '-F')

        if input_path == '-':
            kwargs = dict(stdin=sys.stdin)
        else:
            argv.append(input_path)
            kwargs = dict()
        proc = subprocess.Popen(argv, stdout=subprocess.PIPE, text=True, bufsize=1, **kwargs)
        try:
            yield proc.stdout
        finally:
            if proc.stdout is not None:
                proc.stdout.close()
            status = proc.wait()
            if status not in (0, 1):
                raise RuntimeError(f'{argv[0]} failed with exit status {status}')


def search(args):
    if args.input == '-' and args.patterns_file == '-':
        error("Patterns file and input dump cannot both be read from stdin ('-').")
        sys.exit(2)
    if args.prefilter != 'none' and not shutil.which(args.prefilter):
        error(f'"{args.prefilter}" not found in PATH.')
        sys.exit(2)
    patterns = load_patterns(args.patterns_file)
    if not patterns:
        error("No search patterns provided.")
        sys.exit(3)

    regex, group_to_pattern = compile_patterns_regex(patterns, args.regex)
    counts = {p: 0 for p in patterns}
    hits = {p: [] for p in patterns}

    if args.prefilter != 'none':
        input_file = prefilter_input(args.input, patterns, args.prefilter, args.regex)
    else:
        input_file = fopen(args.input, 'r', encoding='utf-8')

    with input_file as fh, fopen(args.output, 'w') as output_file:
        meta, meta_json, key = None, None, None
        for line in fh:
            if line.startswith(META_PREFIX + '{'):
                meta, meta_json = None, line[len(META_PREFIX):]
                continue
            if regex.search(line) is None:
                continue
            line = line.rstrip('\n')
            if meta is None:
                meta = json.loads(meta_json)
                columns = meta['columns']
                column_index = list(enumerate(columns))
                rindex = {c: i for i, c in column_index}
                key_index = [(rindex[k], k) for k in meta['key']]
            row = line.split('\t')
            formatted_key = format_key(key_index, row)
            for i, col in column_index:
                v = row[i]
                if v:
                    for m in regex.finditer(v):
                        pattern = group_to_pattern[m.lastgroup]
                        counts[pattern] += 1
                        if not args.summary_only:
                            hits[pattern].append((
                                meta['schema'], meta['table'], col, formatted_key,
                                format_snippet(v, m.start(), m.end(), args.context),
                            ))

        # Sort by matches desc; stable tie-breaker by casefolded pattern.
        for pattern in sorted(patterns, key=lambda t: (-counts[t], t.casefold())):
            print(f"{pattern}: {counts[pattern]}", file=output_file)
            for schema, table, col, key, snippet in hits[pattern]:
                print(f"  • {schema}.{table}.{col}, {key}: '{snippet}'", file=output_file)


def main():
    commands = (
        (dump, "Create a searchable dump (JSON metadata + TSV data).", lambda argument: (
            argument('--dbname', '-d', required=True, help="Database name"),
            argument('--user', '-U', '-u', default=getpass.getuser(),
                     help="Database user (default: current system user)"),
            argument('--password', '-W',
                     help="Database password (default: $PGPASSWORD)"),
            argument('--host', '-h',
                     help="Database host (default: localhost)"),
            argument('--port', '-p', type=int, default=5432,
                     help="Database port (default: 5432)"),
            argument('--output', '-o', default='-',
                     help="Output file (default: stdout). Use '-' for stdout."),
            argument('--include-schema', action='append', default=[],
                     help="Include only these schemas (repeatable)."),
            argument('--exclude-schema', action='append', default=[],
                     help="Exclude these schemas (repeatable)."),
            argument('--include-nontext', action='store_true',
                     help="Also dump non-text, non-binary columns for context."),
            argument('--null', default="",
                     help="NULL representation in TSV (default: empty string)."),
        )),
        (search, "Search in a dump produced by the 'dump' command.", lambda argument: (
            argument('patterns_file',
                     help=("File with search patterns, one per line. Empty lines "
                           "and lines starting with # are ignored. Use ‘-’ for stdin.")),
            argument('--input', '-i', default='-',
                     help="Dump file to search through (default: stdin)."),
            argument('--output', '-o', default='-',
                     help="Output file (default: stdout). Use '-' for stdout."),
	    argument('--context', '-c', default=20, type=int,
                     help="Context characters before/after each match."),
	    argument('--regex', '-r', action='store_true',
                     help=("Treat search patterns as regular expressions "
                           "(otherwise fixed substrings). Matches are "
                           "case-insensitive.")),
            argument('--prefilter', choices=('none', 'grep', 'rg'), default='none',
                     help=("Prefilter dumped rows using grep or ripgrep (rg) to speed up "
                           "searching large dumps. Default: none. For correct Unicode "
                           "matching, run under UTF-8 locale (e.g. LANG=en_US.UTF-8).")),
            argument('--summary-only', action='store_true',
                     help="Print only per-pattern match counts (do not list individual hits)."),
        )),
    )
    parser = argparse.ArgumentParser(
        description="Dump searchable columns from PostgreSQL and search the dump.",
        add_help=False,
    )
    parser.add_argument('--help', action='help', help='Show help and exit')
    subparsers = parser.add_subparsers(dest='command_name')
    subparsers.required = True
    for command, help, arguments in commands:
        p = subparsers.add_parser(command.__name__, help=help, add_help=False)
        p.add_argument('--help', action='help', help="Show help and exit")
        arguments(p.add_argument)
        p.set_defaults(command=command)
    args = parser.parse_args()
    args.command(args)


if __name__ == "__main__":
    main()
