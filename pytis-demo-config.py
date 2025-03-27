# -*- coding: utf-8 -*-

from __future__ import print_function

import getpass
import os

application_name = 'Pytis Demo'
search_modules = ('pytis.demo', 'pytis.defs')

dbuser = getpass.getuser()
dbname = 'pytis-demo'
dbhost = None

base_dir = os.path.dirname(__file__)
help_dir = os.path.join(base_dir, 'help')
print_spec_dir = os.path.join(base_dir, 'lib', 'pytis', 'demo', 'output')
