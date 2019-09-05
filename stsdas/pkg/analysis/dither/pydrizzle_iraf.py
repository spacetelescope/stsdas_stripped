from __future__ import division # confidence high

import os
import iraf

no = iraf.no
yes = iraf.yes

import pydrizzle

# Point to default parameter file for task
_parfile = 'dither$pydrizzle.par'
_taskname = 'pydrizzle'

__version__ = "5.5.0 (13-April-2005)"


def _cleanINDEF(iraf_value):
    """ Converts IRAF INDEF to Python None """
    if iraf_value == iraf.INDEF or iraf_value == 'INDEF':
        return None
    return iraf_value

######
# Set up Python IRAF interface here
######
def pydriz_iraf(input,output=None,section='',prodonly=no,kernel='square',units='cps',pixfrac=1.0,
        rotate=no,orient=None,psize=None,ra=None,dec=None,xsize=None,ysize=None,
        bits_single=0,bits_final=0, 
        wt_scl='exptime', in_units='counts', fillval=0.,idckey=None,single=no,
        clean=yes,save=no,build=yes,version=None):
    # Transform IRAF empty parameters to Python None when
    # PyDrizzle expects it.
    if output == '': output = None
    if section == '': section = None
    if kernel == '': kernel = None
    if idckey == '': idckey = None

    if ra == '': ra = None
    if dec == '': dec = None
    if orient == '': orient = None
    if xsize == '': xsize = None
    if ysize == '': ysize = None
    
    if not rotate:
        orient = None
        
    _bits_final = _cleanINDEF(bits_final)
    _bits_single = _cleanINDEF(bits_single)
    # Interpret section parameter as a list
    if section is not None:
        slist = section.split(',')
        section = []
        for s in slist: section.append(int(s))
        #print 'Processing chips: ',section

    iraf.pydrizzle.version = pydrizzle.__version__

    #print 'Running PyDrizzle Version ',version
    # Setup dictionary of parameters used by SkyField object
    _fpars = {'xsize':None,'ysize':None,'psize':None,'orient':None,'ra':None,'dec':None}
    _field=None
    # Check to see if any SkyField parameters were set by user.
    for _par in _fpars.keys():
        _eval = eval(_par)
        # If a parameter was set, then update dictionary with value
        # and set switch
        if _eval != None:
            _fpars[_par] = float(_eval)

    # If we have user-specified parameters for the SkyField object,
    # then setup the object with those values...
    # We are counting how many _fpars values are set to None.
    # If less than full number of _fpars members, then user set something.
    if list(_fpars.values()).count(None) < len(_fpars):
        _field = pydrizzle.pydrizzle.SkyField(None)
        if _fpars['xsize'] != None:
            _shape = (int(_fpars['xsize']),int(_fpars['ysize']))
        else: _shape = None

        _field.set(shape=_shape,psize=_fpars['psize'],orient=_fpars['orient'],
                    ra=_fpars['ra'],dec=_fpars['dec'])
    
    drobj = pydrizzle.PyDrizzle(input,output=output,field=_field,kernel=kernel,section=section,
            units=units,pixfrac=pixfrac,prodonly=prodonly,
            bits_final=_bits_final,bits_single=_bits_single,
            wt_scl=wt_scl, in_units=in_units, fillval=fillval,idckey=idckey)
    drobj.run(save=save,build=build,single=single,clean=clean)
    # Remove intermediate files
    #if clean == yes:
    #    drobj.clean()


# Setup PyDrizzle as an IRAF task here
# by setting up an absolute path to the parfile...
#_ospath = os.path
# File that gets picked up here is: iraffunctions.py
#_abspath = _ospath.split(_ospath.abspath(__file__))[0]
#parfile = os.path.join(_abspath,'pydrizzle.par')

parfile = iraf.osfn(_parfile)
pyd = iraf.IrafTaskFactory(taskname=_taskname, value=parfile, pkgname=PkgName,
            pkgbinary=PkgBinary, function=pydriz_iraf)
iraf.pydrizzle.version = pydrizzle.__version__
