"""
$Revision: 1.3 $ $Date: 2010/01/18 09:16:15 $
Author: Martin Kuemmel (mkuemmel@stecf.org)
Affiliation: Space Telescope - European Coordinating Facility
WWW: http://www.stecf.org/software/slitless_software/axesim/
"""
__author__ = "Martin Kuemmel <mkuemmel@eso.org>"
__date__ = "$Date: 2010/01/18 09:16:15 $"
__version__ = "$Revision: 1.3 $"
__credits__ = """This software was developed by the ACS group of the Space Telescope -
European Coordinating Facility (ST-ECF). The ST-ECF is a department jointly
run by the European Space Agency and the European Southern Observatory.
It is located at the ESO headquarters at Garching near Munich. The ST-ECF
staff supports the European astronomical community in exploiting the research
opportunities provided by the earth-orbiting Hubble Space Telescope.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
"""
class aXeError(Exception):
    """
    Error class for aXe

    This class is just a simple extension to the general exception class.
    All errors specific to 'axe' are thrown using this class to be able
    to distinguish them from different errors.
    """
    def __init__(self, message):
        """
        Initializer for the class

        @param message: message associated to the exception
        @type message: string
        """
        self.message = message

    def __str__(self):
        """
        String method for the class

        @return: the string representation of the class
        @rtype: string
        """
        return self.message

class aXeSIMError(Exception):
    """
    General Error in aXeSIM

    This class is just a simple extension to the general exception class.
    All errors specific to 'axesim' are thrown using this class to be able
    to distinguish them from different errors.
    """
    def __init__(self, message):
        """
        Initializer for the class

        @param message: message associated to the exception
        @type message: string
        """
        self.message = message

    def __str__(self):
        """
        String method for the class

        @return: the string representation of the class
        @rtype: string
        """
        return self.message
