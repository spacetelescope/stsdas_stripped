from __future__ import division         # confidence high
from pyraf import iraf

from calcos import splittag as SPLITTAG
from calcos import calcosparam
_version = calcosparam.CALCOS_VERSION
del (calcosparam)

# Point to default parameter file for task
_parfile = 'hst_calib$hstcos/splittag.par'
_taskname = 'splittag'

######
# Set up Python IRAF interface here
######
def splittag_iraf (input, outroot, starttime, increment, endtime,
                   time_list, verbosity, version=""):

    # Interpret input parameters

    if starttime == iraf.INDEF:
        starttime = None
    if increment == iraf.INDEF:
        increment = None
    if endtime == iraf.INDEF:
        endtime = None
    if time_list.strip() == "":
        time_list=None

    SPLITTAG.splittag (input, outroot=outroot, starttime=starttime,
                       increment=increment, endtime=endtime,
                       time_list=time_list, verbosity=verbosity)

# Initialize IRAF Task definition now...
parfile = iraf.osfn (_parfile)
junk = iraf.IrafTaskFactory (taskname=_taskname, value=parfile,
                             pkgname=PkgName, pkgbinary=PkgBinary,
                             function=splittag_iraf)
iraf.splittag.version = _version
