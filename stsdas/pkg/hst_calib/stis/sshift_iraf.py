from __future__ import division         # confidence high

#  Import IRAF classes
from pyraf import iraf
from pyraf.iraf import stsdas,hst_calib,stis
from stsci.tools.irafglob import irafglob
#import sshift
from stistools import sshift as SH

version = '1.7 (2010-Apr-27)'

# Point to default parameter file for task
_parfile = 'stis$sshift.par'

#####  Initialize Python IRAF interface

def _sshift_iraf(input, output=None, shifts=None, platescale=None,
                 tolerance=None, Version=None):

    words = []
    temp = input.split(",")
    for word in temp:
        words.extend(word.split())
    input = []
    for word in words:
        input.extend(irafglob(word))

    if output == '':
        output = None
    else:
        words = []
        temp = output.split (",")
        for word in temp:
            words.extend (word.split())
        output = words

    if shifts == '':
        shifts = None
    else:
        shifts = [int(v) for v in shifts.split(' ')]

    SH.sshift(input, output=output, shifts=shifts, platescale=platescale,
              tolerance=tolerance)

#  Set up sshift as an IRAF task by setting up an absolute
#  path to the parfile.

dnois = iraf.IrafTaskFactory(taskname='sshift', value=iraf.osfn(_parfile),
                             pkgname=PkgName, pkgbinary=PkgBinary,
                             function=_sshift_iraf)
