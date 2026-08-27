#!/usr/bin/env python3
"""Work with the dependency groups defined in 'pyproject.toml'.

Without options, print the requirements of given group.  Recent pip versions
read the dependency groups (PEP 735) by themselves through the '--group'
option, so this is only needed where pip is too old for that -- the Python 2
test job runs in a container where neither pip nor Python 3 are recent enough.
The output is meant to be piped to 'pip install -r /dev/stdin', so that the
groups remain defined in 'pyproject.toml' only.

With '--check', verify that the requirements of given group are installed in
the current environment and fail with an instructive message if they are not.
This is used by the Makefile to report the missing build tools before they are
actually invoked (see the 'check-build-deps' target).

Usage: dependency-group.py [--check] GROUP [GROUP ...]

"""

# TODO NOPY2: Remove the plain printing mode.  It only exists for the Python 2
# test job -- everywhere else pip reads the dependency groups by itself.

import os
import re
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


def missing(requirements):
    """Return the distribution names of given requirements which are not installed."""
    import importlib.metadata
    result = []
    for requirement in requirements:
        specification, _, marker = requirement.partition(';')
        if marker and not applies(marker):
            continue
        name = re.split(r'[\[<>=!~ (]', specification, 1)[0].strip()
        try:
            importlib.metadata.distribution(name)
        except importlib.metadata.PackageNotFoundError:
            result.append(name)
    return result


def applies(marker):
    """Return true if given environment marker applies to the current environment."""
    try:
        import packaging.markers
    except ImportError:
        # Better to report a dependency which is not actually needed here than
        # to silently ignore a missing one.
        return True
    return packaging.markers.Marker(marker.strip()).evaluate()


def main(argv):
    check = len(argv) > 1 and argv[1] == '--check'
    if check:
        del argv[1]
    if len(argv) < 2:
        raise SystemExit(__doc__.strip())
    path = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                        'pyproject.toml')
    with open(path, 'rb') as f:
        groups = tomllib.load(f).get('dependency-groups', {})
    result = []
    for name in argv[1:]:
        result.extend(r for r in requirements(groups, name) if r not in result)
    if check:
        names = missing(result)
        if names:
            raise SystemExit("Missing dependencies: %s\n"
                             "Activate the virtual environment and install them using:\n"
                             "    pip install %s" %
                             (', '.join(names),
                              ' '.join('--group ' + name for name in argv[1:])))
    else:
        print('\n'.join(result))


if __name__ == '__main__':
    main(sys.argv)
