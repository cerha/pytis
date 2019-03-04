#!/usr/bin/env python

import sys
import os
from distutils.core import setup


def files_list(basedir):
    result = []
    if not basedir:
        return []
    os.chdir(sys.path[0])
    for root, dirs, files in os.walk(basedir, topdown=True):
        for name in files:
            result.append(os.path.join(root, name))
    return result


setup(name='pytis',
      version='1.0',
      description='Pytis libraries',
      author='Tomas Cerha, Milan Zamazal, Pavel Hanak',
      author_email='t.cerha@gmail.com., pdm@zamazal.org, pavel@hanak.name',
      url='https://www.python.org/sigs/distutils-sig/',
      packages=['pytis', 'pytis.form', 'pytis.data', 'pytis.output',
                'pytis.extensions', 'pytis.presentation',
                'pytis.cms', 'pytis.defs', 'pytis.x2goclient',
                'pytis.web', 'pytis.dbdefs', 'pytis.help',
                'pytis.remote', 'pytis.util'],
      package_dir={'': 'lib'},
      scripts=['bin/pytis'] + files_list('./tools'),
      data_files=[('translations', files_list('./translations')),
                  ('icons', files_list('./icons')),
                  ('doc', files_list('./doc')),
                  ('resources', files_list('./resources')),
                  ('help', files_list('./help'))])
