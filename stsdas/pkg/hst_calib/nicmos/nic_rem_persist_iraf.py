from __future__ import division, print_function # confidence high
import os

#  Import IRAF classes
from pyraf import iraf
from pyraf.iraf import stsdas, hst_calib 
from nictools import nic_rem_persist
 
version = '1.1 (2009-Jan-15)'

# Point to default parameter file for task
_parfile = 'nicmos$nic_rem_persist.par'
_taskname = 'nic_rem_persist'

def _nic_rem_persist_iraf(input_file, persist_lo=0.5, used_lo=0.5, persist_model=None, persist_mask=None, verbosity=1 ):

    try:
#       nrp = nic_rem_persist.NicRemPersist( input_file, persist_lo=persist_lo, used_lo=used_lo, persist_model=persist_model, persist_mask=persist_mask, verbosity=verbosity)  
       nrp = nic_rem_persist.NicRemPersist( calcfile, targfile, persist_lo=None, used_lo=None, persist_model=None, persist_mask=None, verbosity=None)  
       nrp.persist()

    except ValueError as msg:
        print("FATAL ERROR: ", msg)
        return

# Set up nic_rem_persist as an IRAF task.
nrpers = iraf.IrafTaskFactory( taskname = _taskname, value = iraf.osfn(_parfile),
                                  pkgname = PkgName, pkgbinary = PkgBinary,
                                  function = _nic_rem_persist_iraf)



