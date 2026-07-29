#!/usr/bin/env python3
"""Print the requirements of given dependency group defined in 'pyproject.toml'.

Recent pip versions read the dependency groups (PEP 735) by themselves through
the '--group' option.  This script is only needed where pip is too old for that
-- the Python 2 test job runs in a container where neither pip nor Python 3 are
recent enough.  The output is meant to be piped to 'pip install -r /dev/stdin',
so that the groups remain defined in 'pyproject.toml' only.

Usage: dependency-group.py GROUP [GROUP ...]

"""

# TODO NOPY2: Remove this script completely.  It only exists for the Python 2
# test job -- everywhere else pip reads the dependency groups by itself.

import os
import sys

try:
    import tomllib
except ImportError:
    import tomli as tomllib  # Python < 3.11


def requirements(groups, name, seen=None):
    """Return the requirements of given group with the included groups expanded."""
    seen = seen if seen is not None else set()
    if name in seen:
        raise SystemExit("Cyclic dependency group: %s" % name)
    seen.add(name)
    try:
        items = groups[name]
    except KeyError:
        raise SystemExit("Dependency group not defined: %s" % name)
    result = []
    for item in items:
        if isinstance(item, dict):
            result.extend(requirements(groups, item['include-group'], seen))
        else:
            result.append(item)
    return result


def main(argv):
    if len(argv) < 2:
        raise SystemExit(__doc__.strip())
    path = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                        'pyproject.toml')
    with open(path, 'rb') as f:
        groups = tomllib.load(f).get('dependency-groups', {})
    result = []
    for name in argv[1:]:
        result.extend(r for r in requirements(groups, name) if r not in result)
    print('\n'.join(result))


if __name__ == '__main__':
    main(sys.argv)
