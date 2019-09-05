from __future__ import division, print_function # confidence high
import os

#  Import IRAF classes
from pyraf import iraf
from pyraf.iraf import stsdas, hst_calib, wfpc
from wfpc2tools import wfpc2destreak

version = '1.2 (2008-September-05)'

# Point to default parameter file for task
_parfile = 'wfpc$wdestreak.par'
_taskname = 'wdestreak'

def _wdestreak_iraf( filename, group=4, row_thresh=0.1,  bias_thresh=100000.0, input_mask=0, niter=5, verbosity=1):


    #  Convert IRAF empty strings to Python None
    if input_mask == "": input_mask = None
    
    try:
        wfpc2_d = wfpc2destreak.Wfpc2destreak( filename,  group=group, \
                                row_thresh=row_thresh,  bias_thresh=bias_thresh, input_mask=input_mask, niter=niter, verbosity=verbosity)
        wfpc2_d.destreak()


    except ValueError as msg:  
        print("FATAL ERROR: ", msg)
        return
 

# Set up Wfpc2destreak as an IRAF task.
wfpc2des = iraf.IrafTaskFactory( taskname = _taskname, value = iraf.osfn(_parfile),
                                  pkgname = PkgName, pkgbinary = PkgBinary,
                                  function = _wdestreak_iraf)
