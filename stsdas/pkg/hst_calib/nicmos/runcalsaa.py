#! /usr/bin/env python
"""
runcalsaa.py - Module to perform SAA correction in the CALNIC pipeline
(After CALNICA, before CALNICB) by running the PEDSUB, BEP, and SAACLEAN tasks.
PEDSUB is run only to improve the calculations of the SAA persistence and BEP
signature; no pedestal correction is actually applied to the final output
image.

USAGE: runcalsaa.py [-d] ipppssoot_raw.fits

Alternative USAGE: python
                   import runcalsaa
                   status=runcalsaa.run('ipppssoot_raw.fits')

RETURN VALUES:
  It will return status codes to indicate completion status:
       0 = successful completion with correction applied
       4 = successful completion with no correction applied
       1 = failed gracefully with exception
       3 = aborted gracefully based on self-diagnostic


REQUIRED INPUT FILES:
  Although these files are not specified on the command line, they
  must be available for the script to succeed.

  In the working directory:
     ipppssoot_cal.fits
     The association file specified in SAA_DARK
     The _raw files specified in that association file

  As specified in the _cal file header:
     SAACNTAB
     PEDSBTAB
     FLATFILE

  As specified in the post-SAA exposure file headers:
     MASKFILE
     SAADFILE

OUTPUT FILES & EFFECTS:
   The ipppssoot_cal.fits file may be replaced.
   The SAADONE keyword in the ipppssoot_cal.fits file is updated.
   The BEPDONE keyword in the ipppssoot_cal.fits file is updated.
   The ipppssoot_trl.txt file is appended to.

INTERIM FILES:
   A _psb.fits file is created temporarily, but removed by the script.
   A _ped2.fits file is created temporarily, but removed by the script.

@author: V. G. Laidler, D. Grumm
@version: 0.4 (3-Jul-2006)
          0.5 (13-Aug-2008)
          1.0 (26-Jan-2009)
          1.1 (29-Jan-2009)
          1.2 (25-Mar-2009)
          1.3 (15-Jun-2010)
          1.4.2 (5-NOv-2013) MLS: changed return codes for opus
"""
from __future__ import print_function

import os,time,sys
from pyraf import iraf
from iraf import stsdas, hst_calib, nicmos,ctools
from iraf import saaclean
from nictools import nic_rem_persist

from astropy.io import fits as pyfits
import numpy as N

__version__ = '1.4.2'
__vdate__ = '25-Nov-2013'
__trlmarker__ = '*** CALNIC RUNCALSAA Processing Version  %s %s ***\n'%(__version__,__vdate__)


"""
These return codes have been changed as requested by opus so that they can detect a return
value of 1 as a real error for the shell script, see #1078
"""
_success  = 0
_none     = 4
_error    = 1
_abort    = 3


# Constants relevant to saaclean
statdict_saaclean = {'none':_none,'low only':_success,'high only':_success,
                      'both':_success,'n/a':_none,'aborted':_abort}

donestring = {_none:'OMITTED',_success:'PERFORMED',_abort:'SKIPPED',
                _error:'SKIPPED'}


def run(rawname,debug=False):

    #............................................................
    # Setup
    #............................................................

    saadone = _none
    bepdone = _none

    if '_raw' not in rawname:
        print("""ERROR: this script takes ipppssoot_raw.fits file as input:
        you provided %s"""%rawname)
        return

    # Define file names
    calname = rawname.replace('_raw','_cal')
    pedname = rawname.replace('_raw','_ped')
    pedname2 = rawname.replace('_raw','_ped2')
    outname = rawname.replace('_raw','_scn_applied')
    saapername = rawname.replace('_raw','_spr')
    pedtrlname = rawname.replace('_raw.fits','_pedsb_trl.txt')

    F_A = calname
    F_B = pedname
    F_C = outname
    F_D = pedname2

    # Establish connection to the trailer file
    trlname = rawname.replace('_raw.fits','_trl.txt')

    Trl = open( trlname,'a')
    Trl.write(_timestamp('RUNCALSAA starting'))
    Trl.write(__trlmarker__)

    # Open the calfile header and determine whether the script should run
    f = pyfits.open(calname)
    prihdr = f[0].header

    # Get some things from the calfile header
    saaparname = f[0].header['saacntab']
    pedparname = f[0].header['pedsbtab']
    camera = f[0].header['camera']

    # Trap the case where no PEDSBTAB was provided, as this reference file is
    # required for running PEDSUB.
    if pedparname == 'N/A':
        # No PEDSUB reference file, so turn off all processing.
        dosaa=False
        saadone=_abort
        dobep=False
        bepdone=_abort
    else:
        if 'saacorr' in prihdr:
            dosaa = (prihdr['saacorr'] == 'PERFORM')
        else:
            dosaa = False
            saadone = _abort

        if 'bepcorr' in prihdr:
            dobep = (prihdr['bepcorr'] == 'PERFORM')
        else:
            dobep = False
            bepdone = _abort

    if ((dosaa or dobep) and (f[0].header['flatdone'] == 'PERFORMED') and (f[0].header['flatfile'] != 'N/A')):
        pass # keep running
    else:
        Trl.write(_timestamp('RUNCALSAA omitted'))
        Trl.close()
        set_keys_final( _abort, _abort, F_A, donestring, saapername)
        # No files to delete
        f.close()
        return _none

    f.close()

    try:  # get pedsub pars for SAACLEAN, BEP, or both
        kwpars = get_pedsub_pars( camera, pedparname, Trl, F_A, saapername, debug=debug)
    except Exception as e:
        handle_exception(e, Trl, [], debug = debug)
        set_keys_final( _abort, _abort, F_A, donestring, saapername )
        # no copy to final  as it already is cal, no files to delete
        return _abort

    if (dosaa):
        if (f[0].header['saadone'] == 'PERFORMED'):
            saadone = _abort
            F_S1 = F_A # set file that is the final for 'stage 1' to file F_A
        else: # f[0].header['saadone'] != 'PERFORMED'):
            try:  # for do_pedsub
                do_pedsub(pedparname, Trl, pedtrlname, F_A, F_B, kwpars, saapername)
            except Exception as e:
                handle_exception(e, Trl, [], debug = debug)
                set_keys_final( _abort, _abort, F_A, donestring,saapername )
                # no copy to final  as it already is cal, no files to delete
                return _abort
            saadone, F_S1 = do_saaclean(F_B, F_A, F_C, trlname, saaparname, camera, saapername, Trl, debug=debug)
    else:   # dosaa is False
        F_S1 = F_A # set file that is the final for 'stage 1' to file F_A

    if (dobep):
        try:
            do_pedsub(pedparname, Trl, pedtrlname, F_S1, F_D, kwpars,saapername)
        except Exception as e:
            handle_exception(e, Trl, [], debug = debug)
            set_keys_final(_abort,_abort, F_A, donestring,saapername )
            # no copy to final  as it already is cal, no files to delete
            return _abort
        bepdone, F_Final = do_bright_ep( F_D, F_S1, Trl, donestring, debug=debug )

    else:   # dobep is False
        F_Final = F_S1

    set_keys_final(saadone, bepdone, F_S1, donestring, saapername)

    os.rename( F_Final, calname)

    Trl.write(_timestamp('RUNCALSAA completed'))
    Trl.close()

    return  _success


def set_keys_final(saadone, bepdone, F_Final, donestring, saapername):
    """ Set values for saadone and bepdone in the final cal file

    @param saadone: value of key SAADONE
    @type saadone: string
    @param bepdone: value of key BEPDONE
    @type bepdone: string
    @param F_Final: name of final cal file
    @type F_Final: string
    @param donestring: mapping of strings for done keys
    @type donestring: dict
    @param saapername: name of persistence model created by SAACLEAN
    @type saapername: string
    """

    fh = pyfits.open( F_Final, mode = 'update' )
    fh[0].header.update('saadone',donestring[saadone])
    fh[0].header.update('bepdone',donestring[bepdone])

    if saapername != None:
       fh[0].header.update('SAACRMAP',saapername)

    fh.close()



def get_pedsub_pars( camera, pedparname, Trl, pedsub_file, saapername, debug=False ):
    """ Get keyword parameter values for pedsub

    @param camera: camera number
    @type camera: int
    @param pedparname: parameter file name
    @type pedparname: string
    @param Trl: trailer file name
    @type Trl: string
    @param pedsub_file: name of file with pedsub pars
    @type pedsub_file: string
    @param saapername: name of file for SAA persistence image
    @type saapername: string
    @return: kwpars
    @rtype: dict
    """

    # Get params from the pedsubtab
    try:
        kwpars = getkwpars(camera,iraf.osfn(pedparname))
    except Exception as e:
        set_keys_final(_error,_error, pedsub_file, donestring,saapername)
        handle_exception(e, Trl, [], debug = debug)
        return _error

    return kwpars


def do_pedsub( pedparname, Trl, pedtrlname, file_1, file_2, kwpars, saapername):
    """ Call pedsub

    @param pedparname: parameter file name
    @type pedparname: string
    @param Trl: trailer file name
    @type Trl: string
    @param pedtrlname: pedsub's trailer file name
    @type pedtrlname: string
    @param file_1: name of input cal file
    @type file_1: string
    @param file_2: name of output ped file
    @type file_2: string
    @param kwpars: keyword params for pedsub
    @type kwpars: dict
    @param saapername: name of file for SAA persistence image
    @type saapername: string
    """
    pedsub_complete='=== PEDSUB finished'

    # Timestamp the trailer file
    Trl.write(_timestamp('PEDSUB starting with paramas from %s'%pedparname))

    # Run pedsub with output directed to special file
    iraf.flprcache()
    iraf.pedsub.unlearn()
    iraf.pedsub(input = file_1, output = file_2, Stdout = pedtrlname, **kwpars)

    # Examine task output & append to trailer file
    pedout = open( pedtrlname )

    for line in pedout:
            Trl.write( line )
    pedout.close()

    os.remove(pedtrlname)

    if not line.startswith(pedsub_complete):
        raise PedsubError

def do_saaclean( calcimage, targimage, output, trlname, saaparname, camera, saapername, Trl, debug=False):
    """ Call saaclean

    @param calcimage: calc file name
    @type calimage: string
    @param targimage: target file name
    @type targimage: string
    @param trlname: trailer file name
    @type trlname: string
    @param saaparname: file name for SAACLEAN pars
    @type saaparname: string
    @param camera: camera number
    @type camera: int
    @param saapername: file name for SAACLEAN persistence
    @type saapername: string
    @param Trl: trailer file
    @type Trl: string
    @return: saadone, stage 1 file
    @rtype: int, string
    """
    Trl.write(_timestamp('SAACLEAN starting from pars in %s'%saaparname))

    # Get the task parameters from the saacntab
    try:
        kwpars = getkwpars( camera,iraf.osfn(saaparname) )
    except Exception as e:
        handle_exception( e, Trl, [calcimage], debug=debug )
        saadone = _error
        return saadone, targimage

    #
    # Run the saaclean task
    try:
        iraf.saaclean.unlearn()
        iraf.saaclean(calcimage = calcimage,
                      targimage = targimage,
                      output = output,
                      saaperfile = saapername,
                      Stderr = Trl, **kwpars)

        retstat = statdict_saaclean[ iraf.saaclean.applied ]

        if not debug:
            if retstat == _abort:
                saadone = _abort
                F_S1 =  targimage #  set file that is the final for 'stage 1' to file targimage
                Trl.write(_timestamp('SAACLEAN aborted'))
                if os.path.exists(output): os.remove(output)
            elif retstat == _none:
                saadone = _none
                F_S1 =  targimage #  set file that is the final for 'stage 1' to file  targimage
                Trl.write(_timestamp('SAACLEAN omitted'))
                if os.path.exists(output): os.remove(output)
            else:  # retstat is SUCCESS
                saadone = _success
                F_S1 = output #  set file that is the final for 'stage 1'
                Trl.write(_timestamp('SAACLEAN completed'))
                fh_targ = pyfits.open(targimage, mode='update')
                fh_targ[0].header.update(key = 'SAACRMAP', value = saapername )
                fh_targ.close()
        else:
            saadone = retstat
            if retstat == _abort or retstat == _none:
                F_S1 = targimage
            else:
                F_S1 = output
                os.rename( targimage,targimage.replace('_cal.','_orig_cal.'))
                os.rename( output,targimage )

        os.remove( calcimage)  # remove ped file (calcimage) because 2nd pedsub will need to write to it

        # Return end of phase 1 final file
        return saadone, F_S1

    except Exception as e:
        if os.path.exists( calcimage ):
            os.remove( calcimage)  # remove ped file (calcimage) because 2nd pedsub will need to write to it

        handle_exception(e, Trl, [calcimage, output], debug = debug)
        saadone = _error
        F_S1 = targimage
        return saadone, targimage


def do_bright_ep( calcimage, targimage, Trl, donestring, debug=False):
    """ Do bright earth persistence correction

    @param calcimage: calc file name
    @type calimage: string
    @param targimage: target file name
    @type targimage: string
    @param Trl: trailer file name
    @type Trl: string
    @return: bepdone, final cal file
    @rtype: int, string
    """

    Trl.write(_timestamp('BEP starting' ))

   # Run the nic_rem_persist task
    try:
        # When nic_rem_persist reset sys.stdout, IPython did not pick up on the
        # change back when nrp.persist() completed, and shut down the entire IPython
        # session when Trl.close() was called.
        # We need to manage sys.stdout here to allow IPython to recognize that
        # we are resetting it back before closing the Trl file.

        sys.orig_stdout = sys.stdout
        sys.stdout = Trl

        nrp = nic_rem_persist.NicRemPersist( calcfile = calcimage, targfile = targimage, run_stdout = None)  # set task's stdout to trailer file
        nrp_stat = nrp.persist()
        bepdone = nrp_stat

        if (donestring[nrp_stat] == 'OMITTED'):
             Trl.write(_timestamp('BEP aborted'))
        elif (donestring[nrp_stat] == 'PERFORMED'):
             Trl.write(_timestamp('BEP completed'))
        else:
             Trl.write(_timestamp('BEP skipped'))

        # Set sys.stdout back to normal now that all Trl messages have been written out
        sys.stdout = sys.orig_stdout

        if os.path.exists( calcimage ):
            os.remove( calcimage)  # remove ped file (calcimage)

        return bepdone, targimage

# If nic_rem_persist fails, we can't proceed. End with an error.
    except Exception as e:
        if os.path.exists( calcimage ):
            os.remove( calcimage)  # remove ped file (calcimage)
        handle_exception(e, Trl, [calcimage], debug = debug)

        # Reset sys.stdout back to normal...
        sys.stdout = sys.orig_stdout

        bepdone = _none
        return bepdone, targimage


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
class PedsubError(Exception):
    def __str__(self):
        return "PEDSUB ended with error"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Utility functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
def handle_exception(e,trl,files_to_delete,debug=False):
    """ Print various useful information to various useful places """
    print(str(e))
    trl.write(_timestamp("Encountered exception"))
    trl.write(str(e))
    if not debug:
        trl.write('\n Cleaning up interim files \n')
        #Clean up files
        for fname in files_to_delete:
            if os.path.isfile(fname):
                os.remove(fname)
    trl.write(_timestamp('RUNCALSAA completed with errors'))

def getkwpars(camera,parname):
    """Extract the correct row of the parameter file based on the
    value of CAMERA. Parameters are returned as a keyword:value
    dictionary."""
    d={}
    f=pyfits.open(parname)
    t=f[1].data
    cols=f[1].columns

    # Pick out the matching row of the "camera" column.
    cams = t.field('camera')
    idx = N.where(cams == camera)[0][0]
    #..........................^^^^^^
    # (The ugly [0][0] syntax is because numarray.where returns
    # a tuple of arrays, and in this case we just want the
    # actual scalar value that can be used to index the other
    # columns in the table).

    for k in cols:
        d[k.name] = t.field(k.name)[idx]
    del d['camera']
    f.close()
    return d

def _timestamp(_process_name):
    """Create formatted time string recognizable by OPUS."""
    _prefix = time.strftime("\n%Y%j%H%M%S-I-----",time.localtime())
    _lenstr = 60 - len(_process_name)
    return _prefix+_process_name+(_lenstr*'-')+'\n'

def _getTime():
    # Format time values for keywords IRAF-TLM, and DATE
    _ltime = time.localtime(time.time())
    time_str = time.strftime('%H:%M:%S (%d-%b-%Y)',_ltime)

    return time_str


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Run from the shell.

if __name__ == '__main__':

    # Look for debug flag
    debug = " -d " in sys.argv

    # Handle arguments
    if len(sys.argv) > 3 or len(sys.argv) < 2:
        print("syntax: runcalsaa.py [-d] inputfilename")
        sys.exit(_error)
    rawname = sys.argv[-1]

    # Run script with error checking
    try:
        retstat = run(rawname,debug=debug)
    except Exception as e:
        print(str(e))
        print("ERROR: RUNCALSAA failed on %s"%rawname)
        retstat = _error

    # Return status
    sys.exit(retstat)
