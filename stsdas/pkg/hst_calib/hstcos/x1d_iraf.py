from __future__ import division         # confidence high
import glob
from pyraf import iraf

from calcos import x1d as X1D
from calcos import calcosparam
_version = calcosparam.CALCOS_VERSION
del (calcosparam)

# Point to default parameter file for task
_parfile = 'hst_calib$hstcos/x1dcorr.par'
_taskname = 'x1dcorr'

def _toBoolean (iraf_flag):
    """Convert IRAF-style yes/no flag to a Python boolean."""

    return (iraf_flag == iraf.yes)

def commaSep (location, dtype=""):
    """Split a string into comma- or blank-separated words.

    @param location: zero or more numerical values, or "none" or "indef"
    @type location: string
    @param dtype: may be "float", "int" or "" (or None); if specified, each
        word in the input location will be converted to the type specified
    @type dtype: string

    @return: if location is empty or blank, the returned value will be None;
        otherwise, the returned value will be a list, each element of which
        will be either a floating-point value or None
    @rtype: list or None
    """

    if location.strip() == "":
        locn = None
    else:
        # first split on comma
        temp_words = location.split (",")
        words = []
        for word in temp_words:
            word = word.strip()
            if word == "":
                words.append (None)
            else:
                # now split on blank
                words.extend (word.split())
        # now check for "none" or "indef", or convert to int or float
        locn = []
        for word in words:
            if word is None:
                locn.append (None)
            else:
                word = word.lower()
                if word == "indef":
                    locn.append (None)
                elif word == "none":
                    locn.append (None)
                else:
                    if dtype and dtype == "float":
                        locn.append (float (word))
                    elif dtype and dtype == "int":
                        locn.append (int (word))
                    else:
                        locn.append (word)

    return locn

######
# Set up Python IRAF interface here
######
def x1d_iraf (input, outdir="", update_input=iraf.no,
              find_target=iraf.no, cutoff=0., location="", extrsize="",
              verbosity=1, version=""):

    # Interpret input parameters

    if outdir.strip() == "":
        outdir=None
    if outdir is not None:
        outdir = iraf.osfn (outdir)

    update_input = _toBoolean (update_input)
    if cutoff <= 0.:
        cutoff = None
    find_targ = {"flag": _toBoolean (find_target), "cutoff": cutoff}

    # convert from string to list
    location = commaSep (location, "float")
    extrsize = commaSep (extrsize, "int")

    inlist = input.split (",")
    more_input = []
    for filename in inlist:
        more_input.extend (filename.split())
    all_input = []
    for filename in more_input:
        filename = iraf.osfn (filename)
        all_input.extend (glob.glob (filename))

    X1D.extractSpec (all_input, outdir=outdir, update_input=update_input,
                     location=location, extrsize=extrsize,
                     find_target=find_targ,
                     verbosity=verbosity)

# Initialize IRAF Task definition now...
parfile = iraf.osfn (_parfile)
junk = iraf.IrafTaskFactory (taskname=_taskname, value=parfile,
                             pkgname=PkgName, pkgbinary=PkgBinary,
                             function=x1d_iraf)
iraf.x1dcorr.version = _version
