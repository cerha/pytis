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
    return [f"{qi(c, connection)}" for c in cols]

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

def build_child_fk_map(connection, foreign_keys):
    """
    Return child_fk_map: child -> list of dicts:
      { 'parent': 'schema.table', 'child_cols': [...], 'parent_cols': [...], 'cons': 'name' }
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

def build_selection_ctes(connection, ordered_tables, start_table, seed_where,
                         child_fk_map, pk_by_table):
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
                f'  SELECT DISTINCT {pk} FROM {schema}.{name}\n'
                + (f'  WHERE {seed_where}\n' if seed_where else '') +
                f')'
            )
        else:
            # derive from all children that reference this parent (and are in the plan)
            derivations = []
            for child, fks in ((ch, fks) for ch, fks in child_fk_map.items() if ch in pk_by_table):
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
                        f'SELECT DISTINCT {pk} FROM {schema}.{name} '
                        f'JOIN {child_schema}.{child_name} ON {on_sql} '
                        f'JOIN {cte_name(child)} ON ({child_pk_qualified}) = ({child_pk_cte})'
                    )

            if not derivations:
                # No child inside the required plan references this table -> nothing is needed
                parts.append(f'{cte_name(t)} ({cte_cols}) AS (SELECT {pk} FROM {schema}.{name} '
                             f'WHERE FALSE)')
            else:
                parts.append(f'{cte_name(t)} ({cte_cols}) AS (\n  ' +
                             '\n  UNION\n  '.join(derivations) + '\n)')
    return 'WITH\n' + ',\n'.join(parts) + '\n', {t: cte_name(t) for t in ordered_tables}

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

    Each dict has the following keys: 'schema', 'name', 'value', 'is_called'.

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
            AND d.deptype = 'a'
            AND (n.nspname || '.' || c.relname) = ANY(%s)
        ORDER BY n.nspname, c.relname, ps.schemaname, ps.sequencename;
    """
    with connection.cursor(cursor_factory=psycopg2.extras.DictCursor) as cur:
        cur.execute(sql, (tables,))
        return cur.fetchall()

def dump_subset(connection, table, seed_where, binary=False, debug=False, debug_sql=False,
                on_conflict_do_nothing=False, full_dump_on_cycles=False, disable_triggers=False,
                force_defer=False):
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
    for scc in sccs:
        if len(scc) > 1:
            cyclic_tables.update(scc)
        else:
            v = next(iter(scc))
            if v in children_sub and v in children_sub[v]:
                cyclic_tables.add(v)

    subset_tables = [t for t in ordered_tables if t not in cyclic_tables]
    full_tables = [t for t in ordered_tables if t in cyclic_tables]

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
                print("   (in cyclic dependency group; may be dumped in full)",
                      file=sys.stderr)
            print(file=sys.stderr)

    if cyclic_tables and not full_dump_on_cycles:
        print("ERROR: Some tables participate in circular foreign-key dependencies "
              "and cannot be safely subsetted:", file=sys.stderr)
        for t in sorted(full_tables):
            print(f" • {t}", file=sys.stderr)
        print("Re-run with --full-dump-on-cycles to dump these tables in full instead "
              "of a minimal subset.", file=sys.stderr)
        sys.exit(4)

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

    if (cyclic_tables and full_dump_on_cycles and
            nondeferrable_cycle_constraints and not force_defer):
        print("ERROR: Some foreign-key constraints on cyclic tables are "
              "NOT DEFERRABLE and would still be checked immediately, even "
              "with SET CONSTRAINTS ALL DEFERRED:", file=sys.stderr)
        for fk in sorted(
            nondeferrable_cycle_constraints.values(),
            key=lambda r: (
                r['child_schema'], r['child_table'], r['constraint_name']
            ),
        ):
            table_name = f"{fk['child_schema']}.{fk['child_table']}"
            print(f" • {table_name}: {fk['constraint_name']}", file=sys.stderr)
        print("Re-run with --force-defer to temporarily drop and recreate "
              "these constraints around the import.",
              file=sys.stderr)
        sys.exit(5)

    if (cyclic_tables and full_dump_on_cycles and
            nondeferrable_cycle_constraints and force_defer):
        constraint_defs = get_constraint_defs(
            connection,
            list(nondeferrable_cycle_constraints.keys()),
        )

    # prepare PKs and FK map
    pk_by_table = {t: get_primary_key(connection, t) for t in ordered_tables}
    child_fk_map = build_child_fk_map(connection, foreign_keys)

    if subset_tables:
        ctes, cte_names = build_selection_ctes(connection, subset_tables, table, seed_where,
                                               child_fk_map, pk_by_table)
    else:
        ctes, cte_names = '', {}

    if cyclic_tables and full_dump_on_cycles:
        print('BEGIN;')
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
                schema = qi(fk['child_schema'], connection)
                name = qi(fk['child_table'], connection)
                consname = qi(fk['constraint_name'], connection)
                print(
                    f'ALTER TABLE {schema}.{name} '
                    f'DROP CONSTRAINT {consname};'
                )

    # Disable user triggers before inserting data (if requested).
    if disable_triggers:
        for t in ordered_tables:
            schema, name = split_table(t)
            print(f'ALTER TABLE {qi(schema, connection)}.{qi(name, connection)} '
                  f'DISABLE TRIGGER USER;')

    fmt = ('BINARY' if binary else 'CSV')
    for t in ordered_tables:
        schema, name = [qi(x, connection) for x in split_table(t)]
        cols = get_table_columns(connection, t)
        cols_list = ', '.join(cols)
        pk = pk_by_table[t]
        pk_list = ', '.join(qi(c, connection) for c in pk)

        # Construct the SELECT that defines which rows to insert.
        select = f'SELECT {cols_list} FROM {schema}.{name}'
        # Use unconditional SELECT for the start table without a seed condition
        # and for tables that are not in subset_tables (e.g. cyclic tables
        # when --full-dump-on-cycles is used).
        if t == table and seed_where:
            # Apply the user-specified condition on the start table.
            select += f' WHERE {seed_where}'
        elif t != table and t in subset_tables:
            # For subsetted tables, restrict rows via CTE-derived primary keys.
            select = f'{ctes}{select} WHERE ({pk_list}) IN (SELECT * FROM {cte_names[t]})'
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
                    values = cur.mogrify(
                        placeholders,
                        [psycopg2.extras.Json(v) if isinstance(v, (dict, list)) else v
                         for v in row],
                    ).decode('utf-8')
                    print(insert + values + on_conflict)
        else:
            # Dump as COPY.
            print(f'COPY {schema}.{name} ({cols_list}) FROM STDIN WITH {fmt};')
            with connection.cursor() as cur:
                cur.copy_expert(f'COPY (\n{select}\n) TO STDOUT WITH {fmt};', sys.stdout)
            print('\.\n')

    # Re-enable triggers after the dump (disabled earlier).
    if disable_triggers:
        for t in ordered_tables:
            schema, name = split_table(t)
            type_trigger = "USER"
            if cyclic_tables and full_dump_on_cycles:
                type_trigger = "ALL"
            print(f'ALTER TABLE {qi(schema, connection)}.{qi(name, connection)} '
                  f'ENABLE TRIGGER {type_trigger};')

    # Re-create the non-deferrable constraints after the dump (dropped earlier).
    if (cyclic_tables and full_dump_on_cycles and
            nondeferrable_cycle_constraints and force_defer):
        for oid, fk in sorted(
            nondeferrable_cycle_constraints.items(),
            key=lambda item: (
                item[1]['child_schema'],
                item[1]['child_table'],
                item[1]['constraint_name'],
            ),
        ):
            schema, tname, cname = [qi(fk[k], connection)
                                    for k in ('child_schema', 'child_table', 'constraint_name')]
            print(f'ALTER TABLE {schema}.{tname} ADD CONSTRAINT {cname} {constraint_defs[oid]};')

    # Adjust sequences for affected tables so that future inserts do not collide.
    for seq in get_owned_sequences_for_tables(connection, list(ordered_tables)):
        schema, name = [qi(seq[k], connection) for k in ('schema', 'name')]
        print(f"SELECT pg_catalog.setval({schema}.{name}, {seq['value']}, true);")

    if cyclic_tables and full_dump_on_cycles:
        print('COMMIT;')


def main():
    parser = argparse.ArgumentParser(description=(
        "Export a minimal, referentially-complete subset of a PostgreSQL database. "
        "Starting from a given table (and optional WHERE condition), the tool finds "
        "all required rows in that table and all recursively referenced parent tables. "
        "The result is emitted in insertion-safe order so it can be imported into a "
        "clean database or merged into an existing one."
    ))
    parser.add_argument("--dbname", required=True,
                        help="Database name")
    parser.add_argument("--user", default=getpass.getuser(),
                        help="Database user (default: current system user)")
    parser.add_argument("--password",
                        help="Database password (default: $PGPASSWORD)")
    parser.add_argument("--host",
                        help="Database host (default: localhost)")
    parser.add_argument("--port", type=int, default=5432,
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
    parser.add_argument("--full-dump-on-cycles", "-F", action='store_true',
                        help=("Allow dumping whole tables when circular foreign-key references "
                              "prevent extracting a minimal subset. Without this option the "
                              "program aborts and shows which tables are affected. Using this "
                              "option may add extra rows to the output."))
    parser.add_argument("--disable-triggers", "-T", action='store_true',
                        help=("Temporarily disable all USER triggers on affected tables "
                              "during data import. Useful when ON INSERT triggers "
                              "would otherwise reject or mutate rows."))
    parser.add_argument("--debug", action='store_true',
                        help="Print table dependency information to STDERR.")
    parser.add_argument("--debug-sql", action='store_true',
                        help="Print the internally generated SQL queries to STDERR.")
    parser.add_argument("--force-defer", "-D", action='store_true',
                        help=("When used with --full-dump-on-cycles, temporarily drop "
                              "non-deferrable foreign-key constraints on cyclic tables "
                              "during import and recreate them afterwards. Data integrity "
                              "is still validated when the constraints are added back."))

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
                    print(f"[ERROR] Table '{table}' does not exist.", file=sys.stderr)
                    sys.exit(3)
                dump_subset(connection, table, args.where, binary=args.binary,
                            debug=args.debug, debug_sql=args.debug_sql,
                            on_conflict_do_nothing=args.on_conflict_do_nothing,
                            full_dump_on_cycles=args.full_dump_on_cycles,
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
                    print("\n[!] Password not provided.", file=sys.stderr)
                    sys.exit(1)
            print(f"[ERROR] PostgreSQL: {e}", file=sys.stderr)
            sys.exit(2)


if __name__ == "__main__":
    main()
