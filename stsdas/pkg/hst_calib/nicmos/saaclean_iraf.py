from __future__ import print_function

import iraf
import os

no = iraf.no
yes = iraf.yes

from nictools import saaclean
from nictools.saaclean import NoPersistError
from nictools.saaclean import BadThreshError
from nictools.saaclean import NegScaleError
from nictools.saaclean import AlreadyDone
from numpy.linalg import LinAlgError 

# Point to default parameter file for task
_parfile = 'nicmos$saaclean.par'
_taskname = 'saaclean'

######
# Set up Python IRAF interface here
#Warning, warning, warning!!! The *order* of the arguments is important.
#It must match the order in which they appear in the parfile.
######
def saaclean_iraf(calcimage, targimage,
                  output,clobber=None,
                  readsaaper=None,saaperfile=None,
                  writesaaper=None,
                  flaatsaaper=None,
                  darkpath=None,scale=None,wf1=None,wf2=None,
                  crthresh=None,noisethresh=None,binsigfrac=None,
                  stepsize=None,
                  fitthresh=None,
                  thresh=None,
                  histbinwidth=None,nclip=None,
                  hirange=None,lorange=None,
                  fitmult=None,applied=None,hi_nr=None,lo_nr=None,
                  alldiags=None,diagroot=None):
    # Don't specify any default values here - they are in the parfile.

    #Grab the iraf symbolset & stuff them in the environment variable.
    if 'nref' not in os.environ:
        os.environ['nref']=iraf.osfn('nref$')
##     if 'nicmos' not in os.environ:
##         os.environ['nicmos']=iraf.osfn('nicmos$')
    if 'nicmos' not in os.environ:
        os.environ['nicmos']=iraf.osfn('nicmos$')

    #If we want to fit the threshold, override any existing value of thresh.
    if fitthresh == yes:
        thresh=None

    #If we want to turn on all the diagnostic files,set up the filenames
    if alldiags:
        outroot='_'+os.path.splitext(os.path.basename(output))[0]
        flatsaaperfile=iraf.osfn(diagroot)+outroot+'_flat_saaper.fits'
        maskfile=iraf.osfn(diagroot)+outroot+'_mask.fits'
        diagfile=iraf.osfn(diagroot)+outroot
    else:
        flatsaaperfile=maskfile=diagfile=None

    #Make sure the darkpath directory exists
    darkpath=iraf.osfn(darkpath)
    if not os.path.isdir(darkpath):
        raise IOError('darkpath = %s does not exist'%darkpath)
        
        
    #Create the parameter structure
    pars=saaclean.params(darkpath=darkpath,
                         saaperfile=saaperfile,
                         clobber=clobber,
                         readsaaper=readsaaper, writesaaper=writesaaper,
                         flatsaaperfile=flatsaaperfile,maskfile=maskfile,
                         scale=scale,wf1=wf1,wf2=wf2,
                         crthresh=crthresh,noisethresh=noisethresh,
                         binsigfrac=binsigfrac,
                         stepsize=stepsize,thresh=thresh,
                         histbinwidth=histbinwidth,
                         nclip=nclip,
                         fitthresh=fitthresh,
                         hirange=hirange,lorange=lorange,
                         dofit=fitmult,diagfile=diagfile)
    #Run the task.
    try:
        saaper,img = saaclean.clean(calcimage,targimage,output,pars)
        #Fill in the output parameters: Do this later!
        try:
            iraf.saaclean.thresh=img.thresh
            iraf.saaclean.applied=img.appstring
            iraf.saaclean.hi_nr=img.domains['high'].nr
            iraf.saaclean.lo_nr=img.domains['low'].nr
        except AttributeError as e:
            print(str(e))
            print("Cannot fill output parameter in iraf task: continuing.")
 
    except NoPersistError as e:
        iraf.saaclean.applied='n/a'
        print("\t %s"%str(e))

    except BadThreshError as e:
        iraf.saaclean.applied='aborted'
        print("\n\t %s"%str(e))
        print("""
        It appears that the fitting process for the threshold has failed.
        You may wish to examine the histogram of the persistence model image,
        and re-run the task, but set the threshold by hand.""")
        print("\nTask aborting...")
    except NegScaleError as e:
        iraf.saaclean.applied='aborted'
        print("\n\t %s"%str(e))
        print("""
        It appears that the fitting process to determine the best scale
        factor for this domain has failed. You may wish to examine the
        contents of the corresponding diag_*signal_domain.dat file (produced if
        saaclean.alldiags = 'yes') to see what's wrong. """)
        print("\nTask aborting...")
    except AlreadyDone as e:
        print("\n\t %s"%str(e))
        print("""
        SCNAPPLD keyword shows that SAA correction has
        already been applied to this file. This task will produce incorrect
        results if performed on an image that has already been corrected.""")
        print("\nTask aborting...")
    except LinAlgError as e:
        print("\n\t %s"%str(e))
        print("""
        Encountered Linear Algebra Error during the gauss-poly fit of
        the SAA persistence model histogram.
           The scaled histogram data that was being fit has been printed
        out in the _gp_hist.txt file for examination.
           This problem sometimes occurs due to a poor selection of
        the histbinwidth parameter; you may want to tweak that value.""")
        print("\nTask aborting...")
        
# Setup Saaclean as an IRAF task here 
# by setting up an absolute path to the parfile...
#_ospath = os.path
# File that gets picked up here is: iraffunctions.py
#_abspath = _ospath.split(_ospath.abspath(__file__))[0]
#parfile = os.path.join(_abspath,'saaclean.par')

parfile = iraf.osfn(_parfile)
pyd = iraf.IrafTaskFactory(taskname=_taskname, value=parfile, pkgname=PkgName, 
            pkgbinary=PkgBinary, function=saaclean_iraf)
