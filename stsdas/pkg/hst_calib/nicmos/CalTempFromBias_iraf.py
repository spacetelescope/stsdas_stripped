from __future__ import division, print_function # confidence high
import os

#  Import IRAF classes
from pyraf import iraf
from pyraf.iraf import stsdas, hst_calib 
from nictools import CalTempFromBias

version = '2.01 (2008-Oct-21)'

# Point to default parameter file for task
_parfile = 'nicmos$CalTempFromBias.par'
_taskname = 'CalTempFromBias'


def _CalTempFromBias_iraf(filename, edit_type="RAW", hdr_key="TFBTEMP", err_key="TFBERR",nref_par= "/grp/hst/cdbs/nref/",
              force=None, noclean=iraf.no, dry_run=1, verbosity=1):

    #  Convert IRAF empty strings to Python None
    if force == '':
        force = None

    try:
        tfb = CalTempFromBias.CalTempFromBias(filename, edit_type=edit_type, hdr_key=hdr_key, err_key=err_key,
                                            nref_par=nref_par, force=force, noclean=noclean, dry_run=dry_run, verbosity=verbosity)
        [temp, sigma, winner, in_flag, dry_run ]= tfb.calctemp()
        stat = tfb.update_header( temp, sigma, winner)

    except ValueError as msg:
        print("FATAL ERROR: ", msg)
        return

# Set up CalTempFromBias as an IRAF task.
tfbias = iraf.IrafTaskFactory( taskname = _taskname, value = iraf.osfn(_parfile),
                                  pkgname = PkgName, pkgbinary = PkgBinary,
                                  function = _CalTempFromBias_iraf)


