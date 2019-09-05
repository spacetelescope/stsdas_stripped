"""
Tweakshifts_iraf.py

September 09, 2005: this version of tweakshifts_iraf.py should be used in conjunction with Version 0.6 of tweak.py
September 26, 2005: this version of tweakshifts_iraf.py should be used in conjunction with Version 0.61 of tweak.py

"""
from __future__ import division, print_function # confidence high

import iraf
import os
from os import path

import tweak

# Point to default parameter file for task
#_parfile = path.expandvars('$MULTIDRIZZLE')+'/multidrizzle.par'
#_parfile = 'mydrizzle$multidrizzle.par'
_parfile = 'dither$tweakshifts.par'
#_parfile = '/hal/data1/tweakshifts/tweakshifts.par'
_taskname = 'tweakshifts'

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
    if value == '':
        return None
    return value


######
# Set up Python IRAF interface here
######
#
# The parameters here MUST be in the same order as in the .par file:
#
def tweak_iraf(input,
                  shiftfile,
                  reference,
                  output,
                  findmode,
                  gencatalog,
                  sextractpars,
                  undistort,
                  computesig,
                  idckey,
                  clean,
                  updatehdr,
                  verbose,
                  catfile,
                  xcol,
                  ycol,
                  fluxcol,
                  fluxmax,
                  fluxmin,
                  fluxunits,
                  nbright,
                  refcat,
                  refxcol,
                  refycol,
                  rfluxcol,
                  rfluxmax,
                  rfluxmin,
                  rfluxunits,
                  refnbright,
                  minobj,
                  nmatch,
                  matching,
                  xyxin,
                  xyyin,
                  tolerance,
                  separation, 
                  fwhmpsf,
                  sigma,
                  datamin,
                  datamax,
                  threshold,
                  nsigma,
                  fitgeometry,
                  function,
                  maxiter,
                  reject,
                  crossref,
                  margin,
                  tapersz,
                  pad,
                  fwhm,
                  ellip,
                  pa,
                  fitbox):

    # Remove IRAF dependencies from parameters.
    _input = input
    _shiftfile = _cleanBlank(shiftfile)
    _reference = _cleanBlank(reference)
    _output = _cleanBlank(output)
    _catfile = _cleanBlank(catfile)
    _refcat = _cleanBlank(refcat)
    _fluxmax = _cleanINDEF(fluxmax)
    _fluxmin = _cleanINDEF(fluxmin)
    _nbright = _cleanINDEF(nbright)
    _rfluxmax = _cleanINDEF(rfluxmax) 
    _rfluxmin = _cleanINDEF(rfluxmin) 
    _refnbright = _cleanINDEF(refnbright)
    _crossref = _cleanBlank(crossref)
    _xyxin = xyxin
    _xyyin = xyyin
    
    _findmode =    findmode
    _gencatalog =  gencatalog
    _undistort =   undistort
    _computesig =  computesig
    _idckey =      idckey
    _clean =       clean
    _updatehdr =   updatehdr
    _verbose =     verbose
    _xcol =        xcol
    _ycol =        ycol
    _fluxcol =     fluxcol
    _refxcol =     refxcol    # dmg changed on 090105 from "= xcol" to " = refxcol 
    _refycol =     refycol    # dmg changed on 090105 from "= ycol" to " = refycol 
    _rfluxcol =    rfluxcol    
    _rfluxunits =  rfluxunits  
    _fluxunits =   fluxunits
    _minobj =      minobj
    _nmatch =      nmatch
    _tolerance =   tolerance
    _separation =  separation
    _fwhmpsf =     fwhmpsf
    _sigma =       sigma
    _datamin =     datamin
    _datamax =     datamax
    _threshold =   threshold
    _nsigma =      nsigma
    _fitgeometry = fitgeometry
    _function =    function
    _maxiter =     maxiter
    _reject =      reject
    _margin =      margin
    _tapersz =     tapersz
    _pad =         pad
    _fwhm =        fwhm
    _ellip =       ellip
    _pa =          pa
    _matching =    matching
    _fitbox =      fitbox
                 
    print('xyxin: ',_xyxin,'   xyyin: ',_xyyin)
    print('fitgeometry: ',_fitgeometry)
    
    try:
        tweak.run(_input,shiftfile=_shiftfile, reference=_reference,
                output=_output, findmode= _findmode, gencatalog=_gencatalog,
                undistort     = _undistort,   
                computesig    = _computesig,
                idckey        = _idckey,  
                clean         = _clean,   
                updatehdr     = _updatehdr,    
                verbose       = _verbose, 
                catfile       = _catfile,    
                xcol          = _xcol,        
                ycol          = _ycol,       
                fluxcol       = _fluxcol,
                fluxmax       = _fluxmax,
                fluxmin       = _fluxmin,
                fluxunits     = _fluxunits,
                nbright       = _nbright, 
                refcat        = _refcat,     
                refxcol       = _refxcol,        
                refycol       = _refycol,       
                rfluxcol      = _rfluxcol,
                rfluxmax      = _rfluxmax,
                rfluxmin      = _rfluxmin,
                rfluxunits    = _rfluxunits,
                refnbright    = _refnbright,      
                minobj        = _minobj,      
                nmatch        = _nmatch,     
                xyxin         = _xyxin,
                xyyin         = _xyyin, 
                tolerance     = _tolerance,  
                fwhmpsf       = _fwhmpsf,     
                sigma         = _sigma,       
                datamin       = _datamin,     
                datamax       = _datamax,     
                threshold     = _threshold,   
                separation    = _separation,
                nsigma        = _nsigma,      
                fitgeometry   = _fitgeometry, 
                function      = _function,    
                maxiter       = _maxiter,     
                reject        = _reject, 
                crossref      = _crossref,    
                margin        = _margin,      
                tapersz       = _tapersz,     
                pad           = _pad,         
                fwhm          = _fwhm,        
                ellip         = _ellip,       
                pa            = _pa,
                matching      = _matching,
                fitbox        = _fitbox)      
                           
    except ValueError as msg:
        print("FATAL ERROR: ", msg)
        return

# IRAF task definition.

parfile = iraf.osfn(_parfile)
tw = iraf.IrafTaskFactory(taskname=_taskname, value=parfile,
                              pkgname=PkgName, pkgbinary=PkgBinary,
                              function=tweak_iraf)
