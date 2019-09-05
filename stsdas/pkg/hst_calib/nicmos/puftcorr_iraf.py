from __future__ import print_function

import iraf
import os

no = iraf.no
yes = iraf.yes

from nictools import puftcorr
from nictools.puftcorr import NoPuftError

# Point to default parameter file for task
_parfile = 'nicmos$puftcorr.par'
_taskname = 'puftcorr'

######
# Set up Python IRAF interface here
######
def puftcorr_iraf(input,output):

    #Grab the iraf symbolset & stuff them in the environment variable.
    if 'nref' not in os.environ:
        os.environ['nref']=iraf.osfn('nref$')
    if 'nicmos' not in os.environ:
        os.environ['nicmos']=iraf.osfn('nicmos$')

    #Run the task.
    try:
        dopuft = puftcorr.clean(input,output)
 
    except NoPuftError as e:
        print(e)
    

# Setup puftcorr as an IRAF task here 
# by setting up an absolute path to the parfile...

parfile = iraf.osfn(_parfile)
pyd = iraf.IrafTaskFactory(taskname=_taskname, value=parfile, pkgname=PkgName, 
            pkgbinary=PkgBinary, function=puftcorr_iraf)
