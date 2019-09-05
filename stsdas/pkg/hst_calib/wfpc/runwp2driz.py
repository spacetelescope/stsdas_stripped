#!/usr/bin/env python

"""
Run PyDrizzle in the WFPC2 pipeline

"""
from __future__ import division, print_function # confidence high

import sys, os.path, time
from stsci.tools import fileutil
from wfpc2tools import wfpc2cte
import pydrizzle
from astropy.io import fits as pyfits
import glob, os


# This version is a branch of v0.7 for initial use in the pipeline
# with makewcs installed as a module. It works with pydrizzle 6.0.1b.

__version__ = "0.9.6 (20-Oct-2009)"
__pydr_version__ = pydrizzle.__version__ 
__cte_version__ = wfpc2cte.__version__

def wf2_pydr(filename):
    trl_start = "PYDRIZZLE STARTED------------ %s ------------\n" % time.asctime()
    
    filename = fileutil.buildRootname(filename, ext=['_c0m.fits','_c0f.fits'])
    try:
        hdr0 = pyfits.getheader(filename)
    except:
        errormsg = "\n\nCan't find file %s \n", filename
        raise IOError(errormsg)
    
    try:
        dodrizcorr = hdr0['DRIZCORR']
    except KeyError:
        print("Keyword DRIZCORR not found\n")
        dodrizcorr = None
    if isSupportedFilter(hdr0) and isSupportedOfftab(hdr0):
        useIDCModel = True
    else:
        useIDCModel = False

    if dodrizcorr != None:
        if dodrizcorr == 'PERFORM':
            try:
                # update WCS information
                if useIDCModel:
                    p = pydrizzle.PyDrizzle(filename, bits_single=0, bits_final=0)
                else:
                    p = pydrizzle.PyDrizzle(filename, bits_single=0, bits_final=0, updatewcs=False,idckey=None)
                # resample to the scale of chip #3
                # this is done in the pipeline to keep the size of the 
                # output drz product small
                chips=[member.chip for member in p.observation.members]
                """
                try:
                    chip_ind = chips.index('3')
                except ValueError:
                    chip_ind = 0
                oscale=p.observation.members[chip_ind].geometry.wcslin.pscale
                """
                # Reset the plate-scale to a fixed value of 0.1 arcsecond/pixel
                # but NOT if the PC was the only chip read out.
                if len(chips) > 1 or (len(chips) == 1 and chips[0] != '1'):
                    skf=pydrizzle.pydrizzle.SkyField()
                    skf.set(psize=0.1)
                    p.resetPars(skf)
                p.run(clean='yes')
                # Clean up any mask files which were created and not deleted by PyDrizzle
                fileutil.removeFile(glob.glob('wfpc2_inmask*.fits'))
                
            except:
                raise 
            #update_header(filename)
            # Update header of output file, not input file
            update_header(p.output)
            trl_end = "END PYDRIZZLE------------ %s ------------\n" % time.asctime()
        else:
            trl_end = 'PYDRIZZLE processing not requested\n'
            trl_end += "END PYDRIZZLE------------ %s ------------\n" % time.asctime()
            return trl_start, trl_end
    else:
        trl_end = 'Keyword DRIZCORR missing from header.\n'
        trl_end += 'Pydrizzle did not run.\n'
    
    
    return trl_start, trl_end

def wf2_cte(filename):
    taskname = "WFPC2CTE"
    trl_start = "%s STARTED------------- %s ------------\n" % (taskname,time.asctime())
    
    filename = fileutil.buildRootname(filename, ext=['_c0f.fits'])

    try:
        hdr0 = pyfits.getheader(filename)
    except:
        errormsg = "\n\nCan't find file %s \n", filename
        raise IOError(errormsg)
    
    if hdr0['INSTRUME'] != 'WFPC2':
        errormsg = "This script should be run on WFPC2 data only.\n"
        raise ValueError(errormsg)
    """
    if hdr0['IMAGETYP'] != 'EXT':
        errormsg = "\n\n%s does not appear to have science observations.\n" % filename
        errormsg += "This script runs only on science data - IMAGETYP -> EXT.\n"
        raise ValueError, errormsg
    """
    
    wfpc2cte.compute_CTE(filename,quiet=False)

    update_cte_header(filename)
    trl_end = "%s FINISHED------------ %s ------------\n" % (taskname,time.asctime())    
    
    return trl_start, trl_end

    
def update_cte_header(filename):
    f = pyfits.open(filename, mode='update')
    hdr0 = f[0].header
    history_str = "WFPC2CTE version %s" % __cte_version__
    hdr0.add_history(history_str)
    f.close()

def isSupportedFilter(hdr):
    idc = hdr['idctab']
    idcname = fileutil.osfn(idc)
    filter1 = hdr['FILTNAM1']
    filter2 = hdr['FILTNAM2']

    try:
        idctab = fileutil.openImage(idcname)
    except:
        raise IOError

    if idctab[1].columns.names.count('FILTER1') > 0 and idctab[1].columns.names.count('FILTER2') > 0:
        # 2 filter IDCTAB, all filter modes should be supported
        val = True        
    else:
        # 1 filter IDCTAB, check to see whether it is a supported filter and 
        # that input is not a 2 filter observation
        filters = idctab[1].data.field('FILTER')
            
        if filter1 not in filters or filter2.strip():
            val = False
        else:
            val = True

    idctab.close()

    return val
def isSupportedOfftab(hdr):
    offname = hdr['offtab']
    if offname == 'N/A':
        return False
    else:
        return True

    
def update_header(filename):
    f = pyfits.open(filename, mode='update')
    hdr0 = f[0].header
    history_str = "PyDrizzle version %s" % __pydr_version__
    hdr0.add_history(history_str)
    hdr0.update('DRIZCORR', 'COMPLETE')
    f.close()
             
def help():
    print("runwp2driz is meant to be run in the WFPC2 pipeline.\n")
    print("It works on calibrated science data in mef format, one file at  time.\n")
    
    


if __name__ == '__main__':
    args = sys.argv[1:]
    input = args[0]
    rootname = input.split('_')[0]
    if len(args) != 1:
        help()
    else:
        wf2_cte(input) 
        wf2_pydr(input)


