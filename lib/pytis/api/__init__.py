# -*- coding: utf-8 -*-

# Copyright (C) 2018-2020 Tomáš Cerha <t.cerha@gmail.com>
# Copyright (C) 2001-2017 OUI Technology Ltd.
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
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""Pytis public API to be used by applications.

This module defines the public API available for Pytis Applications.  The
central point of access to the public API is the 'pytis.api.app' object.  It
can be simply imported in the application code as:

from pytis.api import app

Then the application code can access the public application API methods and
attributes through this 'app' object.

This object exists at the module level even if no actual application is running
(yet).  Pytis will take care of connecting the actual running application
objects (implementing the API) to the 'pytis.api.app' object during
initialization.

The public API for applications is different from the public API which the
Pytis classes expose internally to be accessed from within Pytis itself.
Current Pytis applications often access the internal Pytis API directly.  This
must be always considered a hack and should be gradually moved to access
through this API.  This API needs to be extended in order to allow the features
which are now solved by accessing Pytis internal APIs.  This shold allow us to
get rid of all access to Pytis internals from within applications one day.

As of May 2020, this API is a new thing and is used at just a few places, but as
declared above, the plan is to gradually change all access to Pytis internals from
applications to use this API.

"""

from .api import (
    implements, APIProvider, Application, Form, Field, QueryFields,
)

app = APIProvider()
