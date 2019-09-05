from __future__ import print_function

import os
import sys
import iraf

from axe import __version__ as axe_version

no = iraf.no
yes = iraf.yes

# define the version
# (not yet useful...)

_execfiles = ['aXe_AF2PET','aXe_BE', 'aXe_GOL2AF', 'aXe_PET2SPC',
              'aXe_PETCONT','aXe_PETFF', 'aXe_STAMPS',
              'aXe_DRZPREP', 'aXe_DRZ2PET', 'aXe_GPS', 'aXe_FILET',
              'aXe_NICBACK', 'aXe_DIRIMAGE', 'aXe_SCALEBCK']

_bin_dir = 'axebin$'

_package_motd = """
  The aXe software package (now version %s) was developed by the 
  Slitless Spectroscopy group of the ST-ECF.  Maintenance
  is now provided by the Space Telescope Science Institute.
  Further information is available at:
  http://axe-info.stsci.edu

  Any questions regarding this software can be directed to:
  help@stsci.edu
"""

_abort_msg = """
  The aXe software package  can NOT be loaded at this time!

  At least one of the executables for the aXe software package was
  not found within STSDAS.  Please contact the STScI for a copy
  of the binaries in order to use this package. For details see:
  http://axe-info.stsci.edu
  
  Any questions regarding this software can be directed to:
  help@stsci.edu
"""

# From PyDrizzle.fileutil...
def findFile(input):
    """
    Search a directory for full filename with optional path.
    """
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
#axepath = iraf.envget('axe')
#sys.path.append(os.path.join(axepath,'../'))
#sys.path.append(axepath)
#axepath = iraf.envget('axebin')
#print axepath
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
    # print the banner
    print((_package_motd)%(axe_version))

else:
    # Tell user something was missing
    # and don't define anything...
    print(_abort_msg)
