from __future__ import division         # confidence high
import os
import iraf

no = iraf.no
yes = iraf.yes

import calcos
from calcos import calcosparam

# Point to default parameter file for task
_parfile = 'hst_calib$hstcos/calcos.par'
_taskname = 'calcos'

_version = calcosparam.CALCOS_VERSION

def _toBoolean(iraf_flag):
    """Convert IRAF-style yes/no flag to a Python boolean."""

    return (iraf_flag == yes)

######
# Set up Python IRAF interface here
######
def calcos_iraf(input, verbosity=1, savetmp=no, outdir="",
                find_target=no, cutoff=-1., shift_file="",
                csum=no, raw_csum=no,
                compress=no, comp_param="gzip,-0.01", binx=1, biny=1,
                stimfile="", livefile="", burstfile="", version=None):

    # Interpret input parameters
    find_target = _toBoolean(find_target)
    csum = _toBoolean(csum)
    raw_csum = _toBoolean(raw_csum)
    savetmp = _toBoolean(savetmp)
    compress = _toBoolean(compress)

    if outdir.strip() == "":
        outdir=None
    if outdir is not None:
        outdir = iraf.osfn(outdir)

    if cutoff <= 0.:
        cutoff = None
    if shift_file.strip() == "": shift_file=None
    if stimfile.strip() == "": stimfile=None
    if livefile.strip() == "": livefile=None
    if burstfile.strip() == "": burstfile=None


    calcos.calcos(input, outdir=outdir, verbosity=verbosity,
                  find_target={"flag": find_target, "cutoff": cutoff},
                  create_csum_image=csum,
                  raw_csum_coords=raw_csum,
                  binx=binx, biny=biny,
                  compress_csum=compress,
                  compression_parameters=comp_param,
                  shift_file=shift_file,
                  save_temp_files=savetmp,
                  stimfile=stimfile, livetimefile=livefile,
                  burstfile=burstfile)

# Initialize IRAF Task definition now...
parfile = iraf.osfn(_parfile)
runcal = iraf.IrafTaskFactory(taskname=_taskname,value=parfile,
        pkgname=PkgName, pkgbinary=PkgBinary, function=calcos_iraf)

iraf.calcos.version = _version
