from __future__ import print_function

import iraf
import os

no = iraf.no
yes = iraf.yes

from nictools import rnlincor

# Point to default parameter file for task
_parfile = 'nicmos$rnlincor.par'
_taskname = 'rnlincor'

######
# Set up Python IRAF interface here
######
def rnlincor_iraf(input,output,nozpcorr):

    #Grab the iraf symbolset & stuff them in the environment variable.
    if 'nref' not in os.environ:
        os.environ['nref']=iraf.osfn('nref$')
    if 'nicmos' not in os.environ:
        os.environ['nicmos']=iraf.osfn('nicmos$')

    #Handle optional file specifications
    opt={'nozpcorr':nozpcorr}

    #Run the task.
    try:
        rnlincor.run(input,output,**opt)  
 
    except ValueError as e:
        print("ValueError: ",str(e))

    except KeyError as e:
        print("KeyError: ",str(e))
    

# Setup rnlincor as an IRAF task here 
# by setting up an absolute path to the parfile...

parfile = iraf.osfn(_parfile)
pyd = iraf.IrafTaskFactory(taskname=_taskname, value=parfile, pkgname=PkgName, 
            pkgbinary=PkgBinary, function=rnlincor_iraf)
