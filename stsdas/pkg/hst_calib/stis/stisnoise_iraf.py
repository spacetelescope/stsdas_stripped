from __future__ import division, print_function # confidence high
import os

#  Import IRAF classes
from pyraf import iraf
from pyraf.iraf import stsdas, hst_calib, stis
from stistools import stisnoise as SN

version = '5.5 (2010-Apr-27)'

# Point to default parameter file for task
_parfile = 'stis$stisnoise.par'

#####  Initialize Python IRAF interface

def _stisnoise_iraf(infile, exten=1, outfile=None, freq=0., ampl=0., fwhm=0.,
                    boxcar=0, wipe=None, window=None,
                    dc=iraf.yes, verbose=iraf.yes):
    #  Convert IRAF empty strings to Python None

    if outfile == '':
        outfile = None
    if wipe == '':
        wipe = None
    else:
        wipe = [float(v) for v in wipe.split(' ')]
    if window == '':
        window = None
    else:
        window = [float(v) for v in window.split(' ')]
    if dc == iraf.yes:
        dc = 1
    if verbose == iraf.yes:
        verbose = 1

    f, a = SN.stisnoise(infile, exten=exten, outfile=outfile,
                        dc=dc, verbose=verbose,
                        boxcar=boxcar, wipe=wipe, window=window)

    andx = a.argmax()
    iraf.stisnoise.freq = float(f[andx])
    iraf.stisnoise.ampl = float(a[andx])
    for j in range(andx, len(a)):
        if a[j] < iraf.stisnoise.ampl/2.:
            break
    iraf.stisnoise.fwhm = float(2.*(f[j]-f[andx]))
    if verbose == 1:
        print('FFT Results: max freq: %.4f,  max ampl: %.4f,  fwhm: %.4f' % 
              (iraf.stisnoise.freq, iraf.stisnoise.ampl, iraf.stisnoise.fwhm))

# Set up stisnoise as an IRAF task.

dnois = iraf.IrafTaskFactory(taskname='stisnoise', value=iraf.osfn(_parfile),
                             pkgname=PkgName, pkgbinary=PkgBinary,
                             function=_stisnoise_iraf)
