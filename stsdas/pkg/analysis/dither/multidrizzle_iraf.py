from __future__ import division, print_function # confidence high

import iraf
import os
from os import path

import multidrizzle

# Point to default parameter file for task
#_parfile = path.expandvars('$MULTIDRIZZLE')+'/multidrizzle.par'
#_parfile = 'mydrizzle$multidrizzle.par'
_parfile = 'dither$multidrizzle.par'
_taskname = 'multidrizzle'

no = iraf.no
yes = iraf.yes

def _toBoolean(iraf_flag):
    """ Converts IRAF-style yes/no flag to a Python boolean. """
    # The IRAF booleans are not hashable, thus the ugly
    # conditional construct.
    if (iraf_flag == yes):
        return True
    else:
        return False

def _cleanINDEF(iraf_value):
    """ Converts IRAF INDEF to Python None """
    if iraf_value == iraf.INDEF or iraf_value == 'INDEF':
        return None
    return iraf_value

def _cleanBlank(value):
    """ Converts blank string to Python None """
    if value != None and str(value).strip() == '':
        return None
    return value


######
# Set up Python IRAF interface here
######
#
# The parameters here MUST be in the same order as in the .par file:
#
def mdrizzle_iraf(input,
                  output,
                  mdriztab,
                  refimage,
                  runfile,
                  workinplace,
                  updatewcs,
                  proc_unit,
                  coeffs,
                  context,
                  clean,
                  group,
                  ra,
                  dec,
                  build,
                  shiftfile,
                  staticfile,
                  static,
                  static_sig,
                  skysub,
                  skywidth,
                  skystat,
                  skylower,
                  skyupper,
                  skyclip,
                  skylsigma,
                  skyusigma,
                  skyuser,
                  driz_separate,
                  driz_sep_outnx,
                  driz_sep_outny,
                  driz_sep_kernel,
                  driz_sep_wt_scl,
                  driz_sep_scale,
                  driz_sep_pixfrac,
                  driz_sep_rot,
                  driz_sep_fillval,
                  driz_sep_bits,
                  median,
                  median_newmasks,
                  combine_maskpt,
                  combine_type,
                  combine_nsigma,
                  combine_nlow,
                  combine_nhigh,
                  combine_lthresh,
                  combine_hthresh,
                  combine_grow,
                  blot,
                  blot_interp,
                  blot_sinscl,
                  driz_cr,
                  driz_cr_corr,
                  driz_cr_snr,
                  driz_cr_grow,
                  driz_cr_ctegrow,
                  driz_cr_scale,
                  driz_combine,
                  final_wht_type,
                  final_outnx,
                  final_outny,
                  final_kernel,
                  final_wt_scl,
                  final_scale,
                  final_pixfrac,
                  final_rot,
                  final_fillval,
                  final_bits,
                  final_units, 
                  gain,
                  gnkeyword,
                  rdnoise,
                  rnkeyword,
                  exptime,
                  expkeyword,
                  crbit):

    # Remove IRAF dependencies from parameters.
    _input = input
    _output = _cleanBlank(output)
    _mdriztab = _toBoolean(mdriztab)
    _refimage = refimage
    _runfile = runfile
    _workinplace = _toBoolean(workinplace)
    _context = _toBoolean(context)
    _clean = _toBoolean(clean)
    _group = _cleanBlank(group)
    _ra = _cleanBlank(ra)
    _dec = _cleanBlank(dec)
    _updatewcs = _toBoolean(updatewcs)
    _proc_unit = proc_unit
    _coeffs = _cleanBlank(coeffs)
    _build = _toBoolean(build)
    _shiftfile = _cleanBlank( shiftfile)
    _staticfile = _cleanBlank(staticfile)
    _static_sig = static_sig

    _driz_sep_scale = _cleanINDEF(driz_sep_scale)
    _driz_sep_rot = _cleanINDEF(driz_sep_rot)
    _driz_sep_bits = _cleanINDEF(driz_sep_bits)
    _final_scale = _cleanINDEF(final_scale)
    _final_rot = _cleanINDEF(final_rot)
    _final_bits = _cleanINDEF(final_bits)
    _final_units = _cleanBlank(final_units)   
    _driz_cr_corr = _toBoolean(driz_cr_corr)
    _driz_cr_grow = driz_cr_grow
    _driz_cr_ctegrow = driz_cr_ctegrow
    _crbit = _cleanINDEF(crbit)

    _newmasks   = _toBoolean(median_newmasks)
    _lthresh    = _cleanINDEF(combine_lthresh)
    _hthresh    = _cleanINDEF(combine_hthresh)

    _static           = _toBoolean(static)
    _skysub           = _toBoolean(skysub)
    _skylower         = _cleanINDEF(skylower)
    _skyupper         = _cleanINDEF(skyupper)
    _driz_separate    = _toBoolean(driz_separate)
    _median           = _toBoolean(median)
    _blot             = _toBoolean(blot)
    _driz_cr          = _toBoolean(driz_cr)
    _driz_combine     = _toBoolean(driz_combine)
    
    try:
        md = multidrizzle.Multidrizzle(_input, _output,
            mdriztab = _mdriztab,
            refimage = _refimage,
            runfile = _runfile,
            workinplace = _workinplace,
            coeffs = _coeffs,
            updatewcs = _updatewcs,
            proc_unit = _proc_unit,
            context = _context,
            clean = _clean,
            group = _group,
            ra = _ra,
            dec = _dec,
            build = _build,
            shiftfile = _shiftfile,
            staticfile = _staticfile,
            static = _static,
            static_sig = _static_sig,
            skysub = _skysub,
            skywidth = skywidth,
            skystat  = skystat,
            skylower = _skylower,
            skyupper = _skyupper,
            skyclip  = skyclip,
            skylsigma = skylsigma,
            skyusigma = skyusigma,
            skyuser   = skyuser,
            driz_separate  = _driz_separate,
            driz_sep_outnx = driz_sep_outnx,
            driz_sep_outny = driz_sep_outny,
            driz_sep_kernel = driz_sep_kernel,
            driz_sep_wt_scl = driz_sep_wt_scl,
            driz_sep_scale = _driz_sep_scale,
            driz_sep_pixfrac = driz_sep_pixfrac,
            driz_sep_rot = _driz_sep_rot,
            driz_sep_fillval = driz_sep_fillval,
            driz_sep_bits = _driz_sep_bits,
            median = _median,
            median_newmasks = _newmasks,
            combine_maskpt = combine_maskpt,
            combine_type = combine_type,
            combine_nsigma = combine_nsigma,
            combine_nlow = combine_nlow,
            combine_nhigh = combine_nhigh,
            combine_lthresh = _lthresh,
            combine_hthresh = _hthresh,
            combine_grow = combine_grow,
            blot = _blot,            
            blot_interp = blot_interp,
            blot_sinscl = blot_sinscl,
            driz_cr = _driz_cr,
            driz_cr_corr  = _driz_cr_corr,
            driz_cr_snr   = driz_cr_snr,
            driz_cr_grow   = driz_cr_grow,
            driz_cr_ctegrow  = driz_cr_ctegrow,
            driz_cr_scale = driz_cr_scale,
            driz_combine = _driz_combine,
            driz_final_wht_type = final_wht_type,
            driz_final_outnx = final_outnx,
            driz_final_outny = final_outny,
            driz_final_kernel = final_kernel,
            driz_final_wt_scl = final_wt_scl,
            driz_final_pixfrac = final_pixfrac,
            driz_final_fillval = final_fillval,
            driz_final_scale = _final_scale,
            driz_final_rot = _final_rot,
            driz_final_bits = _final_bits,
            driz_final_units = _final_units,
            gain = gain,
            gnkeyword = gnkeyword,
            rdnoise = rdnoise,
            rnkeyword = rnkeyword,
            exptime = exptime,
            expkeyword = expkeyword,
            crbit = _crbit)
            
    except ValueError as msg:
        print("FATAL ERROR: ", msg)
        return

    # Finally, run it.
    md.build()
    md.run()

# IRAF task definition.

parfile = iraf.osfn(_parfile)
multid = iraf.IrafTaskFactory(taskname=_taskname, value=parfile,
                              pkgname=PkgName, pkgbinary=PkgBinary,
                              function=mdrizzle_iraf)
