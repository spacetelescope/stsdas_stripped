from __future__ import division         # confidence high
from pyraf import iraf
from pyraf.iraf import stsdas,hst_calib,stis
from stistools import mktrace as tr


# Point to default parameter file for task
_parfile = 'stis$mktrace.par'

#####  Initialize Python IRAF interface
#weights must be passed to python in format [(x1,x2), (x3,x4)]

def _mktrace_iraf(input, tracecen=0.0, weights=None):

    if tracecen == None:
        tracecen = 0.0
    if weights == "":
        weights = None
    else:
        wei = [int(t) for i in weights.split(',') for t in i.split(':')]
        i = wei[::2]
        j = wei[1::2]
        weights = [(i[n],j[n]) for n in range(len(i))]

    tr.mktrace(input, tracecen=tracecen, weights=weights)


tracerefine = iraf.IrafTaskFactory(taskname='mktrace', value=iraf.osfn(_parfile), pkgname=PkgName, pkgbinary=PkgBinary, function=_mktrace_iraf)

