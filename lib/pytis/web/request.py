# Copyright (C) 2007, 2011, 2014 Brailcom, o.p.s.
# Author: Tomas Cerha <cerha@brailcom.org>
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

from pytis.util import ProgramError

"""Abstract specification of APIs used by pytis web components to access the HTTP request data."""


class FileUpload(object):
    """Abstract definition of file upload API.

    Instances of 'FileUpload' class should be used to represent file uploads within the request
    parameters.

    """

    def file(self):
        """Return an open file-like object from which the data may be read."""
        raise ProgramError("Pytis FileUpload interface not implemented by derived class!")

    def filename(self):
        """Return the original filename (without path) as a unicode string."""
        raise ProgramError("Pytis FileUpload interface not implemented by derived class!")

    def mime_type(self):
        """Return the mime type provided byt he UA as a string"""
        raise ProgramError("Pytis FileUpload interface not implemented by derived class!")


class Request(object):
    """Abstract definition of an HTTP request API.

    Pytis web components may need to access some information from the HTTP request.  This class
    defines the expected abstract interface.  When a request object is passed to pytis components,
    it must implement this interface.

    """

    def uri(self):
        """Return request URI path relative to server's root.

        The returned URI is a unicode value, which normally starts with a slash
        and continues with an arbitrary numper of path elements separated by
        slashes.  Transfer encoding and HTTP escapes are decoded.

        """
        raise ProgramError("Pytis Request interface not implemented by derived class!")

    def has_param(self, name):
        """Return true if the parameter 'name' was sent with the request."""
        raise ProgramError("Pytis Request interface not implemented by derived class!")

    def param(self, name, default=None):
        """Return the value of request parameter 'name'.

        The returned value is a unicode string (with HTTP escapes decoded) for ordinary parameters,
        a 'FileUpload' instance for uploaded multipart data or a sequence of such values when
        multiple values of the parameter were sent with the request.

        """
        raise ProgramError("Pytis Request interface not implemented by derived class!")

    def cookie(self, name, default=None):
        """Return the value of given cookie as a unicode string.

        The 'default' value is returned if given cookie was not present in the HTTP request.

        """
        raise ProgramError("Pytis Request interface not implemented by derived class!")

    def set_cookie(self, name, value, expires=None):
        """Set the value of given cookie.

        The value of cookie 'name' will be set to a string 'value', with optional expiration time
        set to 'expires' seconds.  This cookie will be sent with the HTTP response for the current
        request.

        """
        raise ProgramError("Pytis Request interface not implemented by derived class!")

    def localizer(self):
        """Return an 'lcg.Localizer()' instance for the current request locales."""
        raise ProgramError("Pytis Request interface not implemented by derived class!")
