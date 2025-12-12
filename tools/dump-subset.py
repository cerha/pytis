#!/usr/bin/env python3
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
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""Dump a minimal, dependency-complete subset of a PostgreSQL table.

Given a starting table and an optional WHERE condition, this tool finds all
rows in that table matching the condition, and all rows in other tables that
are required to satisfy foreign-key references (recursively).  The result is
a minimal, self-contained subset of the database that preserves referential
integrity.

Tables are emitted in the correct insertion order (parents first).  Output can
be generated either as COPY ... FROM STDIN blocks or as INSERT statements with
ON CONFLICT DO NOTHING, depending on the selected options.

This makes it possible to export a consistent data slice from one database and
import it into another, or to merge multiple subsets safely into the same target
database.

"""

import os
import re
import sys
import functools
import getpass
import argparse
import collections
import psycopg2
import psycopg2.extras
import psycopg2.extensions
from collections.abc import Iterable


def split_table(table):
    if '.' in table:
        schema, name = table.split('.', 1)
    else:
        schema, name = 'public', table
    return schema, name


def quote_ident(connection, *parts: str) -> str:
    return '.'.join(psycopg2.extensions.quote_ident(ident, connection) for ident in parts)


def quote_list(connection, idents: Iterable) -> str:
    return ', '.join(quote_ident(connection, *ident) if isinstance(ident, (list, tuple))
                     else quote_ident(connection, ident)
                     for ident in idents)


def quote_table(connection, table: str) -> str:
    return quote_ident(connection, *split_table(table))


def error(*messages):
    print(os.path.basename(sys.argv[0]), ' '.join(sys.argv[1:]), file=sys.stderr)
    print('ERROR:', "\n".join(messages), file=sys.stderr)


def get_foreign_keys(connection):
    sql = """
    SELECT
        nsp_child.nspname  AS child_schema,
        c_child.relname    AS child_table,
        a_child.attname    AS child_col,
        nsp_parent.nspname AS parent_schema,
        c_parent.relname   AS parent_table,
        a_parent.attname   AS parent_col,
        con.conname        AS constraint_name,
        con.condeferrable  AS condeferrable,
        con.condeferred    AS condeferred,
        con.oid            AS constraint_oid,
        ck.ord             AS col_order
    FROM pg_constraint con
    JOIN pg_class c_child   ON c_child.oid = con.conrelid
    JOIN pg_class c_parent  ON c_parent.oid = con.confrelid
    JOIN pg_namespace nsp_child  ON nsp_child.oid = c_child.relnamespace
    JOIN pg_namespace nsp_parent ON nsp_parent.oid = c_parent.relnamespace
    JOIN unnest(con.conkey)  WITH ORDINALITY AS ck(attnum, ord) ON TRUE
    JOIN unnest(con.confkey) WITH ORDINALITY AS fk(attnum, ord) ON fk.ord = ck.ord
    JOIN pg_attribute a_child  ON a_child.attrelid = c_child.oid  AND a_child.attnum = ck.attnum
    JOIN pg_attribute a_parent ON a_parent.attrelid = c_parent.oid AND a_parent.attnum = fk.attnum
    WHERE con.contype = 'f'
    ORDER BY child_schema, child_table, constraint_name, ck.ord;
    """
    with connection.cursor(cursor_factory=psycopg2.extras.DictCursor) as cur:
        cur.execute(sql)
        return cur.fetchall()


def table_exists(connection, table):
    """Return True if the given schema.table exists."""
    schema, name = split_table(table)
    with connection.cursor() as cur:
        cur.execute("""
            SELECT 1
            FROM information_schema.tables
            WHERE table_schema = %s AND table_name = %s;
        """, (schema, name))
        return cur.fetchone() is not None


def get_table_columns(connection, table):
    """Return ordered column names for SELECT/COPY."""
    schema, name = split_table(table)
    with connection.cursor() as cur:
        cur.execute("""
            SELECT column_name
            FROM information_schema.columns
            WHERE table_schema = %s AND table_name = %s
            ORDER BY ordinal_position
        """, (schema, name))
        return [r[0] for r in cur]


def build_parent_graph(fks_rows):
    """Return:
       - parents_by_child[child] -> [...]
       - children_by_parent[parent] -> {child}
    """
    parents_by_child = collections.defaultdict(list)
    children_by_parent = collections.defaultdict(set)
    for fk in fks_rows:
        child = f"{fk['child_schema']}.{fk['child_table']}"
        parent = f"{fk['parent_schema']}.{fk['parent_table']}"
        parents_by_child[child].append((parent, fk['child_col'], fk['parent_col'],
                                        fk['constraint_name'], fk['col_order']))
        children_by_parent[parent].add(child)
    return parents_by_child, children_by_parent


def ancestors_only(start, parents):
    seen, q = set([start]), collections.deque([start])
    while q:
        t = q.popleft()
        for p, *_ in parents.get(t, []):
            if p not in seen:
                seen.add(p)
                q.append(p)
    return seen


def subgraph_edges(nodes, children_map):
    sub = {u: {v for v in children_map.get(u, set()) if v in nodes} for u in nodes}
    for u in nodes:
        sub.setdefault(u, set())
    return sub


def topo_sort(nodes, deps):
    indeg = {u: 0 for u in nodes}
    for u, vs in deps.items():
        for v in vs:
            indeg[v] += 1
    q = collections.deque([u for u in nodes if indeg[u] == 0])
    order = []
    while q:
        u = q.popleft()
        order.append(u)
        for v in deps[u]:
            indeg[v] -= 1
            if indeg[v] == 0:
                q.append(v)
    return order


def get_primary_key(connection, table):
    """Return list of PK columns in order, or [] if no PK."""
    schema, name = split_table(table)
    with connection.cursor() as cur:
        cur.execute("""
            SELECT a.attname
            FROM pg_index i
            JOIN pg_class c ON c.oid = i.indrelid
            JOIN pg_namespace n ON n.oid = c.relnamespace
            JOIN unnest(i.indkey) WITH ORDINALITY AS k(attnum, ord) ON TRUE
            JOIN pg_attribute a ON a.attrelid = c.oid AND a.attnum = k.attnum
            WHERE i.indisprimary AND n.nspname = %s AND c.relname = %s
            ORDER BY k.ord;
        """, (schema, name))
        return [r[0] for r in cur.fetchall()]


def build_child_fk_map(foreign_keys):
    """Return child foreign key map: child -> list of dicts.

    dict = {'parent': 'schema.table',
       'child_cols': [...],
       'parent_cols': [...],
       'cons': 'name',
    }

    Multi-column FKs are grouped and ordered by col_order.
    """
    tmp = collections.defaultdict(lambda: collections.defaultdict(list))
    for fk in foreign_keys:
        child = f"{fk['child_schema']}.{fk['child_table']}"
        parent = f"{fk['parent_schema']}.{fk['parent_table']}"
        tmp[child][(parent, fk['constraint_name'])].append(
            (fk['child_col'], fk['parent_col'], fk['col_order'])
        )
    child_fk_map = collections.defaultdict(list)
    for child, groups in tmp.items():
        for (parent, cons), cols in groups.items():
            cols_sorted = sorted(cols, key=lambda x: x[2])
            child_fk_map[child].append({
                'parent': parent,
                'child_cols': [c for c, _, _ in cols_sorted],
                'parent_cols': [p for _, p, _ in cols_sorted],
                'cons': cons,
            })
    return child_fk_map


def strongly_connected_components(nodes, deps):
    """Return list of SCCs, each as a set of nodes, using Tarjan's algorithm."""
    index = 0
    stack = []
    on_stack = set()
    indices = {}
    lowlink = {}
    sccs = []

    def strongconnect(v):
        nonlocal index
        indices[v] = index
        lowlink[v] = index
        index += 1
        stack.append(v)
        on_stack.add(v)

        for w in deps.get(v, ()):
            if w not in indices:
                strongconnect(w)
                lowlink[v] = min(lowlink[v], lowlink[w])
            elif w in on_stack:
                lowlink[v] = min(lowlink[v], indices[w])

        if lowlink[v] == indices[v]:
            scc = set()
            while True:
                w = stack.pop()
                on_stack.remove(w)
                scc.add(w)
                if w == v:
                    break
            sccs.append(scc)

    for v in nodes:
        if v not in indices:
            strongconnect(v)

    return sccs


def get_constraint_defs(connection, constraint_oids):
    """Return mapping oid -> constraint definition string for given oids."""
    if not constraint_oids:
        return {}
    with connection.cursor() as cur:
        cur.execute("""
            SELECT oid, pg_get_constraintdef(oid, true)
            FROM pg_constraint
            WHERE oid = ANY(%s)
        """, (list(constraint_oids),))
        return {row[0]: row[1] for row in cur}


def get_owned_sequences_for_tables(connection, tables):
    """Return list of dicts for sequences owned by columns of given tables.

    Each dict has the following keys: 'schema', 'name', 'value'.

    Only sequences that have a non-null value (have ever been called) are returned.

    """
    sql = """
        SELECT
            ps.schemaname AS schema,
            ps.sequencename AS name,
            ps.last_value AS value
        FROM pg_class s
             JOIN pg_namespace sn ON sn.oid = s.relnamespace
             JOIN pg_depend d ON d.objid = s.oid
             JOIN pg_class c ON c.oid = d.refobjid
             JOIN pg_namespace n ON n.oid = c.relnamespace
             JOIN pg_sequences ps ON ps.schemaname = sn.nspname AND ps.sequencename = s.relname
        WHERE
            s.relkind = 'S'
            AND d.classid = 'pg_class'::regclass
            AND d.refclassid = 'pg_class'::regclass
            AND d.deptype in ('a', 'i')
            AND (n.nspname || '.' || c.relname) = ANY(%s)
            AND ps.last_value IS NOT NULL
        ORDER BY n.nspname, c.relname, ps.schemaname, ps.sequencename;
    """
    with connection.cursor(cursor_factory=psycopg2.extras.DictCursor) as cur:
        cur.execute(sql, (tables,))
        return cur.fetchall()


def build_selection_sets(connection, reachable_tables, start_table, seed_where,
                         pk_by_table, child_fk_map, debug_sql=False):
    """Create temp selection tables and populate them with PKs to be dumped.

    For each table in reachable_tables, a TEMP table tmp_sel_* is created with the
    same PK columns and types, and filled iteratively so that for every selected
    child row all referenced parent rows are also selected (recursively).

    """
    def dbg_sql(lines, **kwargs):
        sql = ("\n".join(lines) if isinstance(lines, (tuple, list)) else lines).format(**kwargs)
        if debug_sql:
            print(sql, file=sys.stderr)
        return sql
    qi = functools.partial(quote_ident, connection)
    ql = functools.partial(quote_list, connection)
    qt = functools.partial(quote_table, connection)
    selection_tables = {}
    with connection.cursor() as cur:
        # Create selection tables for all reachable tables.
        for t in reachable_tables:
            pk = pk_by_table.get(t)
            if not pk:
                raise ValueError(f"Table has no primary key: {t}")
            schema, name = split_table(t)
            cur.execute("""
                SELECT a.attname, format_type(a.atttypid, a.atttypmod)
                FROM pg_attribute a
                     JOIN pg_class c ON c.oid = a.attrelid
                     JOIN pg_namespace n ON n.oid = c.relnamespace
                WHERE n.nspname = %s
                  AND c.relname = %s
                  AND a.attname = ANY(%s)
            """, (schema, name, pk))
            types = {row[0]: row[1] for row in cur.fetchall()}
            selection_tables[t] = 'tmp_sel_' + re.sub(r'[^a-zA-Z0-9_]', '_', t)
            cur.execute(dbg_sql(
                'CREATE TEMP TABLE {table} ({columns}, PRIMARY KEY ({pk})) ON COMMIT DROP;',
                table=qi(selection_tables[t]),
                columns=', '.join(qi(c) + ' ' + types[c] for c in pk),
                pk=ql(pk),
            ))
        dbg_sql('-----------------------------')

        # Seed start table.
        schema, name = split_table(start_table)
        pk = pk_by_table[start_table]
        cur.execute(dbg_sql(
            ("INSERT INTO {target_table} ({target_columns})",
             "SELECT DISTINCT {columns}",
             "FROM {source_table}",
             "WHERE {where}",
             "ON CONFLICT DO NOTHING;"),
            target_table=qi(selection_tables[start_table]),
            target_columns=ql(pk),
            columns=ql([(schema, name, c) for c in pk]),
            source_table=qi(schema, name),
            where=seed_where,
        ))

        # Iterative closure: propagate from selected child rows to their parents.
        changed = True
        while changed:
            changed = False
            dbg_sql('-----------------------------')
            for child, fks in child_fk_map.items():
                if child not in reachable_tables:
                    continue
                child_pk = pk_by_table[child]
                for fk in fks:
                    parent = fk['parent']
                    if not (parent in reachable_tables and fk['child_cols'] and fk['parent_cols']):
                        continue
                    parent_pk = pk_by_table[parent]
                    cur.execute(dbg_sql(
                        ("INSERT INTO {target_table} ({target_columns})",
                         "SELECT DISTINCT {columns}",
                         "FROM {p} p",
                         "JOIN {c} c ON ({c_fk}) = ({p_fk})",
                         "JOIN {s} s ON ({s_pk}) = ({c_pk})",
                         "ON CONFLICT DO NOTHING;"),
                        target_table=qi(selection_tables[parent]),
                        target_columns=ql(parent_pk),
                        columns=ql(('p', c) for c in parent_pk),
                        p=qt(parent),
                        c=qt(child),
                        s=qi(selection_tables[child]),
                        c_fk=ql(('c', c) for c in fk['child_cols']),
                        p_fk=ql(('p', c) for c in fk['parent_cols']),
                        c_pk=ql(('c', c) for c in child_pk),
                        s_pk=ql(('s', c) for c in child_pk),
                    ))
                    if cur.rowcount:
                        changed = True
                    if debug_sql:
                        print('-- Inserted:', cur.rowcount, file=sys.stderr)
    dbg_sql('-----------------------------')
    return selection_tables


def dump_subset(connection, table, seed_where, binary=False, debug=False, debug_sql=False,
                on_conflict_do_nothing=False, disable_triggers=False, force_defer=False):
    foreign_keys = get_foreign_keys(connection)
    parents, deps = build_parent_graph(foreign_keys)
    reachable_tables = ancestors_only(table, parents)  # table ∪ transitive parents
    children_sub = subgraph_edges(reachable_tables, deps)
    # Order: acyclic tables from topo_sort first, then the rest from reachable_tables (cycles)
    ordered_tables = list(topo_sort(reachable_tables, children_sub))
    ordered_tables += [t for t in reachable_tables if t not in set(ordered_tables)]

    # detect SCCs on the reachable_tables subgraph
    sccs = strongly_connected_components(reachable_tables, children_sub)
    cyclic_tables = set()
    qi = functools.partial(quote_ident, connection)
    ql = functools.partial(quote_list, connection)
    qt = functools.partial(quote_table, connection)
    for scc in sccs:
        if len(scc) > 1:
            cyclic_tables.update(scc)
        else:
            v = next(iter(scc))
            if v in children_sub and v in children_sub[v]:
                cyclic_tables.add(v)

    if debug:
        for t in ordered_tables:
            print(t, file=sys.stderr)
            for fk in foreign_keys:
                parent = f"{fk['parent_schema']}.{fk['parent_table']}"
                child = f"{fk['child_schema']}.{fk['child_table']}"
                if parent == t and child in reachable_tables:
                    print(f" • {child}.{fk['child_col']} → {parent}.{fk['parent_col']}",
                          file=sys.stderr)
            if t in cyclic_tables:
                print("   (in cyclic dependency group)", file=sys.stderr)
            print(file=sys.stderr)

    nondeferrable_cycle_constraints = {}
    constraint_defs = {}
    if cyclic_tables:
        for fk in foreign_keys:
            child = f"{fk['child_schema']}.{fk['child_table']}"
            parent = f"{fk['parent_schema']}.{fk['parent_table']}"
            if child in cyclic_tables or parent in cyclic_tables:
                if not fk['condeferrable']:
                    oid = fk['constraint_oid']
                    if oid not in nondeferrable_cycle_constraints:
                        nondeferrable_cycle_constraints[oid] = fk

    if cyclic_tables and nondeferrable_cycle_constraints and not force_defer:
        error(
            "Some foreign-key constraints on cyclic tables are "
            "NOT DEFERRABLE and would still be checked immediately, even "
            "with SET CONSTRAINTS ALL DEFERRED:",
            '\n'.join([
                f" • {fk['child_schema']}.{fk['child_table']}: {fk['constraint_name']}"
                for fk in sorted(nondeferrable_cycle_constraints.values(),
                                 key=lambda r: (r['child_schema'], r['child_table'],
                                                r['constraint_name']))
            ]),
            "Re-run with --force-defer (-D) to temporarily drop and recreate "
            "these constraints around the import.",
        )
        sys.exit(5)

    if cyclic_tables and nondeferrable_cycle_constraints and force_defer:
        constraint_defs = get_constraint_defs(
            connection,
            list(nondeferrable_cycle_constraints.keys()),
        )

    # prepare PKs and FK map
    pk_by_table = {t: get_primary_key(connection, t) for t in ordered_tables}
    child_fk_map = build_child_fk_map(foreign_keys)

    # build temp selection tables and fill them iteratively
    selection_tables = build_selection_sets(
        connection,
        reachable_tables,
        table,
        seed_where or 'true',
        pk_by_table,
        child_fk_map,
        debug_sql=debug_sql,
    )

    print('BEGIN;')

    # For cyclic tables, wrap the import in a transaction and (optionally) drop FKs.
    if cyclic_tables:
        print('SET CONSTRAINTS ALL DEFERRED;')
        if nondeferrable_cycle_constraints and force_defer:
            for oid, fk in sorted(
                nondeferrable_cycle_constraints.items(),
                key=lambda item: (
                    item[1]['child_schema'],
                    item[1]['child_table'],
                    item[1]['constraint_name'],
                ),
            ):
                print('ALTER TABLE {table} DROP CONSTRAINT {name};'.format(
                    table=qi(fk['child_schema'], fk['child_table']),
                    name=qi(fk['constraint_name']),
                ))

    # Disable user triggers before inserting data (if requested).
    if disable_triggers:
        trigger = "ALL" if cyclic_tables else "USER"
        for t in ordered_tables:
            print(f'ALTER TABLE {qt(t)} DISABLE TRIGGER {trigger};')

    for t in ordered_tables:
        pk = pk_by_table[t]
        columns = get_table_columns(connection, t)
        # SELECT the rows to insert via per-table selection temp table.
        select = ('SELECT {cols} FROM {table} WHERE ({pk}) IN (SELECT {pk} FROM {sel})').format(
            cols=ql(columns),
            table=qt(t),
            pk=ql(pk),
            sel=qi(selection_tables[t]),
        )
        if debug_sql:
            print(select + ';\n', file=sys.stderr)

        if on_conflict_do_nothing:
            if not pk:
                raise ValueError(f"Table has no primary key for ON CONFLICT: {t}")
            with connection.cursor() as cur:
                cur.execute(select)
                for row in cur:
                    print(('INSERT INTO {table} ({cols}) VALUES ({values}) '
                           'ON CONFLICT ({pk}) DO NOTHING;').format(
                               table=qt(t),
                               cols=ql(columns),
                               values=cur.mogrify(
                                   ', '.join(['%s'] * len(columns)),
                                   [psycopg2.extras.Json(v) if isinstance(v, (dict, list)) else v
                                    for v in row],
                               ).decode('utf-8'),
                               pk=ql(pk),
                           ))
        else:
            # Dump as COPY.
            fmt = 'BINARY' if binary else 'CSV'
            print('COPY {table} ({cols}) FROM STDIN WITH {fmt};'.format(
                table=qt(t),
                cols=ql(columns),
                fmt=fmt,
            ))
            with connection.cursor() as cur:
                cur.copy_expert(f'COPY (\n{select}\n) TO STDOUT WITH {fmt};', sys.stdout)
            print('\.\n')

    # Re-enable triggers after the dump (disabled earlier).
    if disable_triggers:
        trigger = "ALL" if cyclic_tables else "USER"
        for t in ordered_tables:
            print(f'ALTER TABLE {qt(t)} ENABLE TRIGGER {trigger};')

    # Re-create the non-deferrable constraints after the dump (dropped earlier).
    if cyclic_tables and nondeferrable_cycle_constraints and force_defer:
        # Finish all deferred constraint checks so there are no pending trigger events.
        print('SET CONSTRAINTS ALL IMMEDIATE;')
        for oid, fk in sorted(
            nondeferrable_cycle_constraints.items(),
            key=lambda item: (
                item[1]['child_schema'],
                item[1]['child_table'],
                item[1]['constraint_name'],
            ),
        ):
            print('ALTER TABLE {table} ADD CONSTRAINT {cname} {cdef};'.format(
                table=qi(fk['child_schema'], fk['child_table']),
                cname=qi(fk['constraint_name']),
                cdef=constraint_defs[oid],
            ))
    # Adjust sequences for affected tables so that future inserts do not collide.
    for seq in get_owned_sequences_for_tables(connection, list(ordered_tables)):
        print("SELECT pg_catalog.setval('{schema}.{name}', {value}, true);".format(**seq))

    print('COMMIT;')


def main():
    parser = argparse.ArgumentParser(description=(
        "Export a minimal, referentially-complete subset of a PostgreSQL database. "
        "Starting from a given table (and optional WHERE condition), the tool finds "
        "all required rows in that table and all recursively referenced parent tables. "
        "The result is emitted in insertion-safe order so it can be imported into a "
        "clean database or merged into an existing one."
    ), add_help=False)
    parser.add_argument('--help', action='help', help='Show help and exit')

    parser.add_argument("--dbname", "-d", required=True,
                        help="Database name")
    parser.add_argument("--user", "-U", "-u", default=getpass.getuser(),
                        help="Database user (default: current system user)")
    parser.add_argument("--password", "-W",
                        help="Database password (default: $PGPASSWORD)")
    parser.add_argument("--host", "-h",
                        help="Database host (default: localhost)")
    parser.add_argument("--port", "-p", type=int, default=5432,
                        help="Database port (default: 5432)")

    parser.add_argument("table",
                        help="Start table (schema.table or just table; default schema = public)")
    parser.add_argument("--where",
                        help="SQL condition limiting the rows selected from the start table.")
    parser.add_argument("--binary", action='store_true',
                        help="Use BINARY COPY format instead of CSV.")
    parser.add_argument("--on-conflict-do-nothing", "-i", action='store_true',
                        help=("Output INSERT statements with ON CONFLICT (pk) DO NOTHING "
                              "instead of COPY, allowing safe merges into an existing database."))
    parser.add_argument("--disable-triggers", "-T", action='store_true',
                        help=("Temporarily disable all USER triggers on affected tables "
                              "during data import. Useful when ON INSERT triggers "
                              "would otherwise reject or mutate rows."))
    parser.add_argument("--debug", action='store_true',
                        help="Print table dependency information to STDERR.")
    parser.add_argument("--debug-sql", action='store_true',
                        help="Print the internally generated SQL queries to STDERR.")
    parser.add_argument("--force-defer", "-D", action='store_true',
                        help=("When cyclic tables participate in foreign-key cycles with "
                              "NOT DEFERRABLE constraints, temporarily drop those "
                              "constraints around the import and recreate them afterwards. "
                              "Data integrity is still validated when the constraints are "
                              "added back."))

    args = parser.parse_args()
    table = args.table if "." in args.table else f"public.{args.table}"
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
                if not table_exists(connection, table):
                    error(f"Table '{table}' does not exist.")
                    sys.exit(3)
                dump_subset(connection, table, args.where, binary=args.binary,
                            debug=args.debug, debug_sql=args.debug_sql,
                            on_conflict_do_nothing=args.on_conflict_do_nothing,
                            disable_triggers=args.disable_triggers,
                            force_defer=args.force_defer)
            break
        except psycopg2.OperationalError as e:
            msg = str(e).lower()
            if password is None and ("password authentication failed" in msg
                                     or "no password supplied" in msg
                                     or "authentication failed" in msg
                                     or getattr(e, "pgcode", None) in ("28P01", "28000")):
                try:
                    password = getpass.getpass("Password: ")
                    continue
                except (EOFError, KeyboardInterrupt):
                    error("Password not provided.")
                    sys.exit(1)
            error(f"PostgreSQL: {e}")
            sys.exit(2)


if __name__ == "__main__":
    main()
