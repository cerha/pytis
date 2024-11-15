#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Sample pytis script.

This script can be invoked from a command-line (or a cron job or whatever).
You can call any non-interactive application defined function this way.

The path to pytis configuration file must be set in environment variable
'PYTISCONFIG'.

"""

import pytis
from pytis.demo.misc import mark_products

# Disable the pytis notification thread.
pytis.config.dblisten = False

count = mark_products(mark=False)
