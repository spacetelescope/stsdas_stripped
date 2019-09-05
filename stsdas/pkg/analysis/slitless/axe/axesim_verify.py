"""
$Revision: 1.1 $ $Date: 2010/05/18 07:56:00 $
Author: Martin Kuemmel (mkuemmel@stecf.org)
Affiliation: Space Telescope - European Coordinating Facility
WWW: http://www.stecf.org/software/slitless_software/axesim/
"""
from __future__ import print_function

import os, sys
import iraf

no  = iraf.no
yes = iraf.yes

# define the version
# (not yet useful...)
__version__ = '1.4'

_execfiles = ['aXe_AF2PET', 'aXe_GOL2AF', 'aXe_PET2SPC',
              'aXe_PETCONT', 'aXe_STAMPS',
              'aXe_DIRIMAGE']

_bin_dir = 'axesimbin$'

_package_motd = """
  The aXeSIM software package 1.4 was developed by the
  Slitless Spectroscopy Group of the ST-ECF.  Maintenance
  is provided by the Space Telescope Science Institute. 
  Further information is available at:
  http://www.stsci.edu/resources/software_hardware/stsdas/axe

  Any questions regarding this software can  be directed to:
  help@stsci.edu
"""

_abort_msg = """
  The aXeSIM software package can NOT be loaded at this time!
  In case of persistent problems, check the aXe webpages for
  information at:
  http://www.stsci.edu/resources/software_hardware/stsdas/axe

  Any questions regarding this software can  be directed to:
  help@stsci.edu
"""

# From PyDrizzle.fileutil...
def findFile(input):

    """ Search a directory for full filename with optional path. """

    _fdir,_fname = os.path.split(input)

    if _fdir == '':
        _fdir = os.curdir

    flist = os.listdir(_fdir)

    found = no
    for name in flist:
        if not name.find(_fname):
            found = yes
            continue
    return found


#--------------------------------------------------------------
#
# The main part starts here:
#
#axesimpath = iraf.envget('axesim')
#sys.path.append(axesimpath)
#print sys.path

#
# Check to see that all the executables are present
#
_present = yes
for bin in _execfiles:
    _fname = iraf.osfn(_bin_dir+bin)
    if findFile(_fname) == no:
        _present = no

# Now, decide whether to define the package or not
if _present == yes:
    # All binaries are present, so...
    # ... acknowledge those responsible for the software
    # and where users can get support, then...    
    print(_package_motd)

else:
    # Tell user something was missing
    # and don't define anything...
    print(_abort_msg)
