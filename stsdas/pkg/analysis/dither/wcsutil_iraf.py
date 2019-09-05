from __future__ import division, print_function # confidence high

import os,string
import iraf

from pydrizzle import xytosky
no = iraf.no
yes = iraf.yes


# Point to default parameter file for task
_parfile = 'dither$xytosky.par'
_taskname = 'xytosky'
__version__ = '1.1 (12-Feb-2007)'

######
# Set up Python IRAF interface here
######
def XYtoSky_iraf(input,x=None,y=None,coords=None,colnames=None,linear=yes,
                idckey='IDCTAB',hms=no,output=None,ra=None,dec=None,
                verbose=yes):

    # Transform IRAF empty parameters to Python None when expected.
    if coords == '': coords = None
    if colnames == '': colnames = None
    if idckey == '': idckey = None
    if output == '': output = None    
    
    if not coords and x == None:
        print('Please specify either a single position or a file with positions.')
        return
    
    ra,dec = xytosky.XYtoSky_pars(input,x=x,y=y,coords=coords,colnames=colnames,
                         linear=linear,idckey=idckey,hms=hms,
                         output=output,verbose=verbose)
        
    # Update IRAF parameters with new values
    if not coords:
        iraf.xytosky.ra = ra[0]
        iraf.xytosky.dec = dec[0]
    
# Setup PyDrizzle as an IRAF task here 
parfile = iraf.osfn(_parfile)
xys = iraf.IrafTaskFactory(taskname=_taskname, value=parfile, pkgname=PkgName, 
            pkgbinary=PkgBinary, function=XYtoSky_iraf)
