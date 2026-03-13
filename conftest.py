import sys

# pytis.rest requires Python 3 (type-union syntax, dataclasses with slots, …).
# Exclude it from collection entirely when running under Python 2 so that
# pytest does not choke on syntax errors before any test starts.
if sys.version_info[0] < 3:
    collect_ignore_glob = ['pytis/rest/*']
