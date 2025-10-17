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
import sys
import getpass
import argparse
import collections
import psycopg2
import psycopg2.extras


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

def table_exists(connection, table):
    """Return True if the given schema.table exists."""
    if "." in table:
        schema, name = table.split(".", 1)
    else:
        schema, name = "public", table
    with connection.cursor() as cur:
        cur.execute("""
            SELECT 1
            FROM information_schema.tables
            WHERE table_schema = %s AND table_name = %s;
        """, (schema, name))
        return cur.fetchone() is not None

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


def main():
    parser = argparse.ArgumentParser(description=(
        "Starting from the specified table find all tables that it depends "
        "on through foreign key references and prints them in the correct "
        "insertion order (parents first, start table last). It also shows "
        "which child tables reference each parent."
    ))
    parser.add_argument("--dbname", required=True, help="Database name")
    parser.add_argument("--user", default=getpass.getuser(), help="Database user (default: current user)")
    parser.add_argument("--password", help="Database password (default: $PGPASSWORD)")
    parser.add_argument("--host", help="Database host (default: localhost)")
    parser.add_argument("--port", type=int, default=5432, help="Database port (default: 5432)")
    parser.add_argument("table", help="Start table (schema.table or table; default schema = public)")
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
                foreign_keys = get_foreign_keys(connection)
            break
        except psycopg2.OperationalError as e:
            msg = str(e).lower()
            if password is None and ("password authentication failed" in msg
                                     or "no password supplied" in msg
                                     or "authentication failed" in msg
                                     or getattr(err, "pgcode", None) in ("28P01", "28000")):
                try:
                    password = getpass.getpass("Password: ")
                    continue
                except (EOFError, KeyboardInterrupt):
                    print("\n[!] Password not provided.", file=sys.stderr)
                    sys.exit(1)
            print(f"[ERROR] PostgreSQL: {e}", file=sys.stderr)
            sys.exit(2)

    parents, deps = build_parent_graph(foreign_keys)
    required = ancestors_only(table, parents)  # table ∪ transitive parents
    dep_sub = subgraph_edges(required, deps)
    tables, has_cycles = topo_sort(required, dep_sub)

    if has_cycles:
        print("WARNING: Dependency graph cycles detected — deferred constraints necessary!\n\n")
        tables = dfs_postorder_ancestors(table, parents)  # deterministic fallback

    for t in tables:
        if t in required:
            print(t)
            for fk in foreign_keys:
                parent = f"{fk['parent_schema']}.{fk['parent_table']}"
                child  = f"{fk['child_schema']}.{fk['child_table']}"
                if parent == t and child in required:
                    print(f" - {child}.{fk['child_col']} → {parent}.{fk['parent_col']}")


if __name__ == "__main__":
    main()
