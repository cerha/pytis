#!/usr/bin/env python

import sys


def _usage():
    print 'usage: genconfig.py file'
    sys.exit(1)


def go():
    if len(sys.argv) != 2:
        _usage()
    outfile = sys.argv[1]
    sys.argv = sys.argv[:1]
    import pytis.util  # noqa: F401 (importing for the side effect of 'config' module init)
    f = open(outfile, 'w')
    import config
    config.dump_config_template(f)
    f.close()


if __name__ == '__main__':
    go()
