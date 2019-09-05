#!/usr/bin/env python

""" runtempfrombias.py - Module to control operation of CaltTempFromBias in the CALNIC pipeline

USAGE: runtempfrombias.py inputFile


Alternative USAGE:  python
                    import runtempfrombias
                    status=runtempfrombias.TempFromBias(inputFile)

RETURN VALUES:
  It will return status codes to indicate completion status:
       0 = successful completion with calculations performed
       1 = successful completion with no calculations performed

REQUIRED INPUT FILE:
  Although this file is not specified on the command line, it
  must be available for the script to succeed.

  In the $nref directory:
       The nonlinearity file specified by the 'NLINFILE' keyword
          in the input raw file primary header


OUTPUT FILES & EFFECTS:
  If calculations are performed, the keywords TFBTEMP, TFBERR, TFBVER,
     TFBDATE, and TFBMETH in the primary extension of the input raw file
     will be updated
  If calculations are not performed, the keyword TFBDONE will be reset to 'SKIPPED'
  If calculations are performed, the keyword TFBDONE will be reset to 'PERFORMED'


@author: D. Grumm
@version: 2.0 (19-Sep-2008)

""" 

from __future__ import division, print_function # confidence high
import sys, os.path, time
from nictools import CalTempFromBias,tfbutil

import pydrizzle, pyfits

import stsci.tools as pytools
from stsci.tools import parseinput


__version__ = "2.0" 
__vdate__ = '19-Sep-2008'
__trlmarker__ = '*** CALNIC RUNTEMPFROMBIAS Processing Version  %s %s ***\n'%(__version__,__vdate__)

_success = 0
_none    = 1
_error   = 2 

def run(filename, verbosity=1):

    #............................................................
    #Setup
    #............................................................
    donestring={_success:'PERFORMED',_none:'OMITTED', _error:'SKIPPED'}

    if '_raw' not in filename:
        print("""ERROR: this script takes a raw.fits file as input: you provided %s"""%filename)
        return
   
    # Establish connection to the trailer file
    trlname = filename.replace('_raw.fits','_trl.txt')
    Trl = open(trlname,'a')
    Trl.write(_timestamp('RUNTEMPFROMBIAS starting'))
    Trl.write(__trlmarker__)

    # Open the file primary header and determine whether the script should run
    fh = pyfits.open(filename) 
    if (fh[0].header['TFBCALC'] == 'PERFORM'):
        pass #keep running
    else:
        Trl.write(_timestamp('RUNTEMPFROMBIAS omitted'))
        Trl.close()
        return _none


    # run the task
    try:
        tfb = CalTempFromBias.CalTempFromBias(filename)
        
        [temp, sigma, winner, in_flag, dry_run ]= tfb.calctemp()

        if temp != None: 
            stat = tfb.update_header( temp, sigma, winner)

        Trl.write(' Input file: %s' %filename)
        Trl.write('\n Edit type: %s' %tfb.edit_type)   
        Trl.write('\n Header key: %s' %tfb.hdr_key)   
        Trl.write('\n Error key: %s' %tfb.err_key)   
        Trl.write('\n Force: %s' %tfb.force)   
        Trl.write('\n Noclean: %s' %tfb.noclean)   
        Trl.write('\n Calculated temperature: %s' %temp)
        Trl.write('\n Sigma: %s' %sigma )
        Trl.write('\n Algorithm: %s' %winner)
        Trl.write(_timestamp('RUNTEMPFROMBIAS completed'))


    except Exception as e:
        handle_exception(e,Trl)
        Trl.write(_timestamp('RUNTEMPFROMBIAS aborted'))
        return _error


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Utility functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def handle_exception(e,trl):
    """ Print various useful information to various useful places """
    print(str(e))
    trl.write(_timestamp("Encountered exception"))
    trl.write(str(e))
    trl.write(_timestamp('RUNTEMPFROMBIAS completed with errors'))

def _timestamp(_process_name):
    """Create formatted time string recognizable by OPUS."""
    _prefix= time.strftime("\n%Y%j%H%M%S-I-----",time.localtime())
    _lenstr = 60 - len(_process_name)
    return _prefix+_process_name+(_lenstr*'-')+'\n'


if __name__=="__main__":  

    args = sys.argv[1:]
    _nargs = len(sys.argv)

    if _nargs != 2 :
        print("syntax: runtempfrombias.py inputFilename")
        sys.exit()

    filename = args[0]

    try:
         retstat =  run( filename)
    except Exception as e:
         print(str(e))
         print("ERROR: RUNTEMPFROMBIAS failed on %s"%filename)
         retstat=_error

 # Return status
    sys.exit (retstat)     



