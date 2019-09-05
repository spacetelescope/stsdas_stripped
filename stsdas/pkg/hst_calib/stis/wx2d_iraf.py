from __future__ import division         # confidence high
from pyraf import iraf
from pyraf.iraf import stsdas, hst_calib, stis
from stistools import wx2d as WX

version = "1.2 (2010 April 27)"

def _wx2d_iraf (input, output="", wavelengths="",
                helcorr="perform",
                # algorithm="wavelet",
                trace="", order=7, subdiv=8,
                psf_width=0., rows="",
                subsampled="", convolved=""):

    #  Convert IRAF values to Python values.
    if wavelengths.strip() == "":
        wavelengths = None
    helcorr = helcorr.strip()
    algorithm = "wavelet"
    if algorithm != "wavelet" and algorithm != "kd":
        raise ValueError("algorithm can only be 'wavelet' or 'kd'")
    if trace.strip() == "":
        trace = None
    if subsampled.strip() == "":
        subsampled = None
    if convolved.strip() == "":
        convolved = None
    rows = convert_rows (rows)

    WX.wx2d (input, output=output, wavelengths=wavelengths,
             helcorr=helcorr, algorithm=algorithm,
             trace=trace, order=order, subdiv=subdiv,
             psf_width=psf_width, rows=rows,
             subsampled=subsampled, convolved=convolved)

_parfile = "stis$wx2d.par"

t_wx2d = iraf.IrafTaskFactory (taskname="wx2d", value=iraf.osfn(_parfile),
                               pkgname=PkgName, pkgbinary=PkgBinary,
                               function=_wx2d_iraf)

def convert_rows (rows):
    """Read a two-element tuple from a string.
rows should be a string containing two integers separated by a
comma, blank, or colon.  The numbers may be enclosed in parentheses
or brackets, but this is not necessary.  Note:  the row numbers
are one indexed and inclusive, e.g. rows = "480, 544" means process
rows 479 through 543 (zero indexed), which is equivalent to the
slice 479:544.
"""

    if rows.strip() == "":
        rows = None
    else:
        bad = True
        if rows.find (",") >= 0:
            rownum = rows.split (",")
        else:
            rownum = rows.split (" ")
        if len (rownum) == 2:
            bad = False
            try:
                row0 = int (rownum[0]) - 1
                row1 = int (rownum[1])
            except:
                bad = True
        if bad:
            raise ValueError("can't interpret rows = %s" % (rows,))
        rows = (row0, row1)

    return rows
