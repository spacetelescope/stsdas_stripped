from __future__ import division, print_function # confidence high

import os, glob
import iraf

no = iraf.no
yes = iraf.yes

from stsci.tools import asnutil 

# Point to default parameter file for task
_parfile = 'dither$buildasn.par'
_taskname= 'buildasn'
######
# Set up Python IRAF interface here
######
def buildasn_iraf(asnroot,suffix=None,shiftfile=None,verbose=no,product="",version=None):
    # Transform IRAF empty parameters to Python None when
    # PyDrizzle expects it.
    if suffix == '': 
        suffix = "*crj.fits"
    else:
        suffix = "*" + suffix 
        

    if shiftfile == '': shiftfile = None
    input = glob.glob(suffix)
    if verbose:
        print('Running asnutil version ',asnutil.__version__)
                
    #dthprod =buildasn.buildAsnTable(asnroot,suffix=suffix,shiftfile=shiftfile,verbose=verbose)
    print('input, asnroot, ', input, asnroot)
    asndict = asnutil.ASNTable(input,output=asnroot,shiftfile=shiftfile)
    asndict.create()
    if shiftfile:
        asndict.update(shiftfile=shiftfile)
    asndict.write()

    iraf.buildasn.product = asndict['output']
# Setup this module as an IRAF task here 
# by setting up an absolute path to the parfile...
#_ospath = os.path
# File that gets picked up here is: iraffunctions.py
#_abspath = _ospath.split(_ospath.abspath(__file__))[0]
#parfile = os.path.join(_abspath,'pydrizzle.par')

parfile = iraf.osfn(_parfile)
pyd = iraf.IrafTaskFactory(taskname=_taskname, value=parfile, pkgname=PkgName, 
            pkgbinary=PkgBinary, function=buildasn_iraf)
