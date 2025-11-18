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

"""List tables needed to preserve foreign key integrity on insertion.

Starting from the specified table, the script finds all tables
that it depends on through foreign key references and prints them
in the correct insertion order (parents first, start table last).
It also shows which child tables reference each parent.

Use this to copy or export a subset of data into a new database
while keeping all required referenced records.

If the reachable subgraph is acyclic, output topological order (parents →
children).  If cycles exist, warn and output BFS order (insertion order
is irrelevant with deferred constraints).

"""

import os
import re
import sys
import getpass
import argparse
import collections
import psycopg2
import psycopg2.extras
from psycopg2.extensions import quote_ident as qi


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

def split_table(table):
    if '.' in table:
        schema, name = table.split('.', 1)
    else:
        schema, name = 'public', table
    return schema, name

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
    """Return ordered, quoted column identifiers for SELECT/COPY."""
    schema, name = split_table(table)
    with connection.cursor() as cur:
        cur.execute("""
            SELECT column_name
            FROM information_schema.columns
            WHERE table_schema = %s AND table_name = %s
            ORDER BY ordinal_position
        """, (schema, name))
        cols = [r[0] for r in cur]
    # quote each identifier
    return [f"{qi(c, connection)}" for c in cols]

def build_parent_graph(fks_rows):
    """Return:
       - parents[child] -> [(parent, child_col, parent_col, constraint_name, col_order)]
       - deps[parent] -> {child}  (for topo-sort when restricted to ancestors)
    """
    import collections
    parents, deps = collections.defaultdict(list), collections.defaultdict(set)
    for fk in fks_rows:
        child = f"{fk['child_schema']}.{fk['child_table']}"
        parent = f"{fk['parent_schema']}.{fk['parent_table']}"
        parents[child].append((parent, fk['child_col'], fk['parent_col'],
                               fk['constraint_name'], fk['col_order']))
        deps[parent].add(child)
    return parents, deps

def ancestors_only(start, parents):
    seen, q = set([start]), collections.deque([start])
    while q:
        t = q.popleft()
        for p, *_ in parents.get(t, []):
            if p not in seen:
                seen.add(p)
                q.append(p)
    return seen

def subgraph_edges(nodes, deps):
    sub = {u: {v for v in deps.get(u, set()) if v in nodes} for u in nodes}
    for u in nodes:
        sub.setdefault(u, set())
    return sub

def topo_sort(nodes, deps):
    indeg = {u: 0 for u in nodes}
    for u, vs in deps.items():
        for v in vs:
            indeg[v] += 1
    q, order = collections.deque([u for u in nodes if indeg[u] == 0]), []
    while q:
        u = q.popleft()
        order.append(u)
        for v in deps[u]:
            indeg[v] -= 1
            if indeg[v] == 0:
                q.append(v)
    return order, len(order) != len(nodes)

def dfs_postorder_ancestors(start, parents):
    sys.setrecursionlimit(10000)
    seen, visiting, out = set(), set(), []
    def visit(t):
        if t in seen or t in visiting:
            return
        visiting.add(t)
        for p, *_ in parents.get(t, []):
            visit(p)
        visiting.remove(t)
        seen.add(t)
        out.append(t)
    visit(start)
    return out  # parents first, start last

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

def build_fk_map_grouped(connection, foreign_keys):
    """
    Return child_fks: child -> list of dicts:
      { 'parent': 'schema.table', 'child_cols': [...], 'parent_cols': [...], 'cons': 'name' }
    Multi-column FKs are grouped and ordered by col_order.
    """
    import collections
    tmp = collections.defaultdict(lambda: collections.defaultdict(list))
    for fk in foreign_keys:
        child  = f"{fk['child_schema']}.{fk['child_table']}"
        parent = f"{fk['parent_schema']}.{fk['parent_table']}"
        tmp[child][(parent, fk['constraint_name'])].append(
            (fk['child_col'], fk['parent_col'], fk['col_order'])
        )
    child_fks = collections.defaultdict(list)
    for child, groups in tmp.items():
        for (parent, cons), cols in groups.items():
            cols_sorted = sorted(cols, key=lambda x: x[2])
            child_fks[child].append({
                'parent': parent,
                'child_cols': [c for c, _, _ in cols_sorted],
                'parent_cols': [p for _, p, _ in cols_sorted],
                'cons': cons
            })
    return child_fks

def copy_command(connection, table, where_clause=None, with_header=False):
    """
    Return a server-side COPY TO STDOUT statement selecting explicit columns,
    optionally with a WHERE clause string (e.g. 'id IN (..)' or 'id = 42').
    """
    schema, name = [qi(x, connection) for x in split_table(table)]
    cols = ', '.join(get_table_columns(connection, table))
    where   = f' WHERE {where_clause}' if where_clause else ''
    header  = ' HEADER' if with_header else ''
    return f'COPY (SELECT {cols} FROM {schema}.{name}{where}) TO STDOUT WITH CSV{header};'

def build_selection_ctes(connection, ordered_tables, start_table, seed_where, child_fks, pk_by_table):
    """
    Return a single string like:
      WITH sel_schema_table(pk1, pk2, ...) AS (...),
           sel_parent(...) AS (...),
           ...
    CTE order is from child to parent (reverse of insertion order) so dependencies are defined.
    """
    def cte_name(t):  # stable, readable CTE identifier
        return 'sel_' + re.sub(r'[^a-zA-Z0-9_]', '_', t)

    parts = []
    # Build in reverse: start first, then its parents, etc.
    for t in reversed(ordered_tables):
        schema, name = [qi(x, connection) for x in split_table(t)]
        cols = pk_by_table.get(t)
        if not cols:
            raise ValueError(f"Table has no primary key: {t}")
        cte_cols = ', '.join(qi(c, connection) for c in cols)
        pk = ', '.join(f'{schema}.{name}.{qi(c, connection)}' for c in cols)

        if t == start_table:
            parts.append(
                f'{cte_name(t)} ({cte_cols}) AS (\n'
                f'  SELECT DISTINCT ({pk}) FROM {schema}.{name}\n'
                + (f'  WHERE {seed_where}\n' if seed_where else '') +
                f')'
            )
        else:
            # derive from all children that reference this parent (and are in the plan)
            derivations = []
            for child, fks in ((ch, fks) for ch, fks in child_fks.items() if ch in pk_by_table):
                for fk in fks:
                    if fk['parent'] != t:
                        continue
                    if child not in set(ordered_tables):
                        continue

                    child_schema, child_name = [qi(x, connection) for x in split_table(child)]

                    # build equality join: child.fk_cols = parent.ref_cols
                    on_pairs = [
                        f'{child_schema}.{child_name}.{qi(c, connection)}'
                        f' = {schema}.{name}.{qi(p, connection)}'
                        for c, p in zip(fk['child_cols'], fk['parent_cols'])
                    ]
                    on_sql = ' AND '.join(on_pairs)

                    child_pk = pk_by_table.get(child, [])
                    # tuple of child PK columns qualified with table alias
                    child_pk_qualified = ', '.join(
                        f"{child_schema}.{child_name}.{qi(c, connection)}" for c in child_pk
                    )
                    # child PK columns qualified with the child's CTE
                    child_pk_cte = ', '.join(
                        f'{cte_name(child)}.{qi(c, connection)}' for c in child_pk
                    )
                    derivations.append(
                        f'SELECT DISTINCT ({pk}) FROM {schema}.{name} '
                        f'JOIN {child_schema}.{child_name} ON {on_sql} '
                        f'JOIN {cte_name(child)} ON ({child_pk_qualified}) = ({child_pk_cte})'
                    )

            if not derivations:
                # No child inside the required plan references this table -> nothing is needed
                parts.append(f'{cte_name(t)} ({cte_cols}) AS (SELECT {pk} FROM {schema}.{name} WHERE FALSE)')
            else:
                parts.append(
                    f'{cte_name(t)} ({cte_cols}) AS (\n  ' +
                    '\n  UNION\n  '.join(derivations) + '\n)'
                )
    return 'WITH\n' + ',\n'.join(parts), {t: cte_name(t) for t in ordered_tables}

def dump_subset(connection, table, seed_where, binary=False, debug=False, debug_sql=False,
                on_conflict_do_nothing=False):
    foreign_keys = get_foreign_keys(connection)
    parents, deps = build_parent_graph(foreign_keys)
    required = ancestors_only(table, parents)  # table ∪ transitive parents
    dep_sub = subgraph_edges(required, deps)
    tables, has_cycles = topo_sort(required, dep_sub)

    # prepare PKs and FK map
    pk_by_table = {t: get_primary_key(connection, t) for t in tables}
    child_fks   = build_fk_map_grouped(connection, get_foreign_keys(connection))
    ctes, cte_names = build_selection_ctes(connection, tables, table, seed_where, child_fks, pk_by_table)

    fmt = ('BINARY' if binary else 'CSV')

    if has_cycles:
        if debug:
            print("WARNING: Dependency graph cycles detected — deferred constraints necessary!\n",
                  file=sys.stderr)
        print('BEGIN;\nSET CONSTRAINTS ALL DEFERRED;')
        tables = dfs_postorder_ancestors(table, parents)  # deterministic fallback

    for t in tables:
        if t in required:
            if debug:
                print(t, file=sys.stderr)
                for fk in foreign_keys:
                    parent = f"{fk['parent_schema']}.{fk['parent_table']}"
                    child  = f"{fk['child_schema']}.{fk['child_table']}"
                    if parent == t and child in required:
                        print(f" * {child}.{fk['child_col']} → {parent}.{fk['parent_col']}",
                              file=sys.stderr)
                print(file=sys.stderr)

            schema, name = [qi(x, connection) for x in split_table(t)]
            cols = get_table_columns(connection, t)
            cols_list = ', '.join(cols)
            pk = pk_by_table[t]
            pk_list = ', '.join(qi(c, connection) for c in pk)
            # Construct the SELECT that defines which rows to insert.
            if t == table:
                where = f' WHERE {seed_where}' if seed_where else ''
            else:
                where = f' WHERE ({pk_list}) IN (SELECT * FROM {cte_names[t]})'
            select = f'{ctes}\nSELECT {cols_list} FROM {schema}.{name}{where}'
            if debug_sql:
                print(select + ';\n', file=sys.stderr)

            if on_conflict_do_nothing:
                if not pk:
                    raise ValueError(f"Table has no primary key for ON CONFLICT: {t}")
                insert = f'INSERT INTO {schema}.{name} ({cols_list}) VALUES '
                placeholders = '(' + ', '.join(['%s'] * len(cols)) + ')'
                on_conflict = f' ON CONFLICT ({pk_list}) DO NOTHING;'
                with connection.cursor() as cur:
                    cur.execute(select)
                    for row in cur:
                        values = cur.mogrify(placeholders, row).decode('utf-8')
                        print(insert + values + on_conflict)
            else:
                # Dump as COPY.
                print(f'COPY {schema}.{name} ({cols_list}) FROM STDIN WITH {fmt};')
                with connection.cursor() as cur:
                    cur.copy_expert(f'COPY (\n{select}\n) TO STDOUT WITH {fmt};', sys.stdout)
                print('\.\n')

    if has_cycles:
        print('COMMIT;')


def main():
    parser = argparse.ArgumentParser(description=(
        "Starting from the specified table find all tables that it depends on "
        "through foreign key references and dump all data needed to restore "
        "the subset of the specified (start) table data matching the given "
        "condition. The tables are dumped in the correct insertion order "
        "(parents first, start table last) and only records needed by the "
        "start table subset are present."
    ))
    parser.add_argument("--dbname", required=True, help="Database name")
    parser.add_argument("--user", default=getpass.getuser(),
                        help="Database user (default: current system user)")
    parser.add_argument("--password", help="Database password (default: $PGPASSWORD)")
    parser.add_argument("--host", help="Database host (default: localhost)")
    parser.add_argument("--port", type=int, default=5432, help="Database port (default: 5432)")
    parser.add_argument("table",
                        help="Start table (schema.table or table; default schema = public)")
    parser.add_argument("--where",
                        help="SQL condition limiting the records dumped from the start table.")
    parser.add_argument("--binary", action='store_true',
                        help="Dump data in BINARY COPY format (default: CSV).")
    parser.add_argument("--on-conflict-do-nothing", "-i", action='store_true',
                        help=("Output INSERT statements with ON CONFLICT (pk) DO NOTHING "
                              "instead of COPY, allowing safe merges into an existing database."))
    parser.add_argument("--debug", action='store_true',
                        help="Print information about table relations to STDERR.")
    parser.add_argument("--debug-sql", action='store_true',
                        help="Print the SQL commands gathering the data subsets to STDERR.")
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
                password=password
            ) as connection:
                if not table_exists(connection, table):
                    print(f"[ERROR] Table '{table}' does not exist.", file=sys.stderr)
                    sys.exit(3)
                dump_subset(connection, table, args.where, binary=args.binary,
                            debug=args.debug, debug_sql=args.debug_sql,
                            on_conflict_do_nothing=args.on_conflict_do_nothing)
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
                    print("\n[!] Password not provided.", file=sys.stderr)
                    sys.exit(1)
            print(f"[ERROR] PostgreSQL: {e}", file=sys.stderr)
            sys.exit(2)


if __name__ == "__main__":
    main()
