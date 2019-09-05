"""
Tweak.py - Compute the residual offsets between input images and 
            a reference image after applying the shifts reported in
            their WCS keywords.  This task will generate a shiftfile,
            ASCII file with residual shifts used as input for PyDrizzle
            and MultiDrizzle, as output.
            
Version 0.1 (WJH) - Initial Version, Warren J. Hack, October 2004.
Version 0.1.1 (WJH) - Import 'noao' to insure that digiphot can be loaded

Version 0.1.3 (CJH) - Modified the calling sequence for Pydrizzle to use
    bits_single = None, and bits_final=None.
    
Version 0.1.4 (WJH) - Generalized the naming convention for cross-corr products.
Version 0.2.0 (WJH) - Use of SExtractor started to be implemented, and 
            reference WCS now defaults to [wcs] extension.
Version 0.3.1 (WJH) - Added 'idckey' parameter to interface to support IDCTAB
            use with NICMOS data.
Version 0.4.0 (WJH) - Iteration using 'shiftfile' as an input has been implemented.
                        Requires a new input parameter 'shiftfile' and PyDrizzle 5.5.4. 
Version 0.5   (WJH) - Added 'scale' as output column for shiftfile, corrected problems with
                    reference file usage for cross-corr case.
Version 0.6   (WJH,DMG)- External reference catalog can now be given as the reference
                        frame for all input images.
Version 0.61  (DMG) - Exposed xyxymatch parameter 'matching', so this can now be set by user.
    
"""
from __future__ import division, print_function # confidence high

import os,copy
import iraf 
from iraf import noao,images,immatch,digiphot,daophot
from iraf import stsdas,analysis,fourier,tables
from iraf import dither
from stsci.tools import irafglob, parseinput, wcsutil, fileutil
from astropy.io import fits as pyfits

try:
    import sextractor
except:
    sextractor = None
    
from stsci.tools import wcsutil
import pydrizzle 
import buildasn, updateasn

import updatehdr

yes = iraf.yes
no = iraf.no

__version__ = '0.8.1 (19 Aug 2010)'

DEFAULT_PARS = {'gencatalog':'sextractor','fwhmpsf':2.5, 'sigma':0.0, 
                'datamin':'INDEF', 'datamax':'INDEF', 
                'readnoise':0.0, 'epadu':1.0, 'threshold':4.0, 'nsigma':1.5,
                'nmatch':30,  'tolerance': 1.0, 'separation':0.0, 'fitgeometry':'rotate', 
                'function':'polynomial', 'maxiter':0, 'reject':3.0,
                'dinp':no, 'dref':no, 'pad':no, 'margin':50, 'tapersz':50,
                'verbose':no, 'minobj':20, 'undistort':yes, 'clean':yes,
                'catfile':None, 'xcol':1, 'ycol':2, 'fluxcol':3, 
                'xyxin':"INDEF",'xyyin':"INDEF",
                'fluxmax':None, 'fluxmin':None, 'fluxunits':'mag', 'nbright':None, 
                'computesig':yes, 'updatehdr':no,
                'xc_fwhm':7.0, 'xc_ellip':0.05, 'xc_pa':45., 'xc_fitbox':7,
                'rfluxmax':None, 'rfluxmin':None, 'rfluxunits':'mag', 'refnbright':None,
                'refcat':None, 'refxcol':1, 'refycol':2, 'rfluxcol':3,'matching':'tolerance',}
                
CROSSCORR_PARS = ['dinp', 'dref','pad', 'margin','tapersz',
                   'xc_fwhm', 'xc_ellip', 'xc_pa', 'xc_fitbox',
                   'verbose', 'clean']
MATCH_PARS = ['gencatalog','minobj','fwhmpsf', 'sigma', 'datamin', 'datamax', 'readnoise', 
                'epadu', 'threshold', 'nsigma', 'nmatch', 'tolerance', 'separation',
                'fitgeometry', 'function', 'maxiter', 'reject',
                'verbose', 'undistort', 'catfile','xcol','ycol', 'xyxin','xyyin', 
                'fluxcol', 'fluxmax', 'fluxmin', 'fluxunits', 'nbright', 
                'clean','computesig','idckey', 'shiftfile',
                'refcat','refxcol','refycol','rfluxcol','matching',     
                'rfluxmax','rfluxmin','rfluxunits','refnbright']

def removeFile(fname):
    """ Utility function to safely remove given filename."""
    if fname != None and os.path.exists(fname):
        os.remove(fname)

def buildName(fname,keepext=no,coord_suffix = '.coord'):
    """ For a given filename, build filename for the coords file. """
    
    froot,fext = fileutil.parseFilename(fname)
    froot_name = froot[:froot.rfind('.')]

    if keepext == no:
        fcoord_name = froot_name+coord_suffix
    else:
        if fext != None:
            fext = fext.replace(',','')
        else:
            fext = ''
        fcoord_name = froot_name+'_'+fext+coord_suffix
    
    removeFile(fcoord_name)    
    
    return fcoord_name

def buildNewAsnTable(flist,reference, outroot):
    """ Build a new association table for given input file list and
        output rootname.
    """
    files = copy.copy(flist)
    if files.count(reference) == 0:
        files.insert(0,reference)
    # At this point, input is simply a list of image names, which need to be
    # turned into an ASN file for use with PyDrizzle.
    asnid = outroot+'_asn.fits'
    # Insure that this ASN file does not already exist
    removeFile(asnid)
    # Build new (temporary) ASN file
    asn = buildasn.buildAsnTable(outroot,suffix=files)
    
    return asnid
    
def parseInputPars(input_dict,in_list):
    """ Parse out any user-supplied parameters
         and use default values for remainder of values.
         This function will parse out only those pars given
         by the 'in_list' parameter.
    """
    input_pars = {}
    for par in in_list:
        if par in input_dict: 
            input_pars[par] = input_dict[par]
        else: 
            input_pars[par] = DEFAULT_PARS[par]
    
    return input_pars

def define_output_name(fname):
    """Determines the output name based on input filename or drizzle keyword """
    phdr = pyfits.getheader(fname,ext=0)
    if 'D001DATA' in phdr:
        outname = phdr['D001DATA']
        if outname.find('['):outname = outname.split('[')[0]
    else:
        frootname = fname.split('_')[0]
        outname = fileutil.buildRootname(frootname)
        if outname is None:
            # make one from the header keywords
            outname = phdr['rootname']
    del phdr

    return outname
    
def readDBFile(dbfile):
    """ Read GEOMAP output database file and extract shift info. """
    
    xsh = ysh = 0.0
    rot = 0.0
    scale = 1.0
    if os.path.exists(dbfile):
        # Open file and read in all lines
        db = open(dbfile,mode='r')
        dblines = db.readlines()

        for line in dblines:
            # Find line with xshift and yshift labels
            if line.find('xshift') > -1:
                xsh = float(line.split()[1])
            elif line.find('yshift') > -1:
                ysh = float(line.split()[1])
            elif line.find('yrotation') > -1:
                rot = float(line.split()[1])
            elif line.find('xmag') > -1:
                scale = float(line.split()[1])
            else:
                pass
        # Close the file
        db.close()

    return xsh,ysh,rot,scale
    
def readCoordCatalog(fname,xcol=1,ycol=2,fluxcol=None):
    """ Read target positions from coordinate catalog.  """
    targlist = []
    fluxlist = []
    if os.path.exists(fname):
        # Open file and read in all lines
        tfile = open(fname,mode='r')
        tlines = tfile.readlines()
        
        for line in tlines:
            if line[0] != '#' and line.strip() != '':
                line = line.split()
                targlist.append((float(line[xcol-1]),float(line[ycol-1])))
                if fluxcol != None:
                    fluxlist.append(float(line[fluxcol-1]))
                else:
                    fluxlist.append(0.0)
        # Close the file
        tfile.close()
    
    return targlist,fluxlist
    
def readCatFile(fname):
    """ Read the file which contains the names of the catalog file
        for each input.
    """
    catdict = {}
    if os.path.exists(fname):
        cname = open(fname,mode='r')
        clines = cname.readlines()
        
        for line in clines:
            if line[0] != '#' and line.strip() != '':
                csplit = line.split()
                catdict[csplit[0]] = csplit[1]
    return catdict
 
def countObj(coord_name):
    """ Count the number of objects found for catalog."""

    num_obj = 0

    if os.path.exists(coord_name):
        fcoo = open(coord_name)
        flines = fcoo.readlines()
        for line in flines:
            if line[0] != '#' and line.strip() != '': num_obj += 1
        fcoo.close()
        
    return num_obj


def writeCoordFile(coordlist, fluxlist, fname):
    """ Write out undistorted target positions. """

    # Remove any previous file with the same name
    removeFile(fname)
    
    # Write out preface information 
    preface_str =  "# Shift file generated using tweakshifts version "+__version__+"\n"
    preface_str += "# Col 1: undistorted X position\n"
    preface_str += "# Col 2: undistorted Y position\n"
    preface_str += "# Col 3: Magnitude/flux \n"
    
    
    # Open file and read in all lines
    cfile = open(fname,mode='w')
    cfile.writelines(preface_str)
    
    for n in range(len(coordlist)):
        coord = coordlist[n]
        flux = fluxlist[n]
        cstr = str(coord[0])+'    '+str(coord[1])+'    '+str(flux) +'\n'
        cfile.write(cstr)
        
    cfile.close()

def writeExtnameFile(extlist, fname):
    """ Write out @file with extension filenames.  """

    # Remove any previous file with the same name
    removeFile(fname)
    
    # Open file and read in all lines
    cfile = open(fname,mode='w')
    
    for extn in extlist:
        cfile.write(extn+'\n')
        
    cfile.close()
    
       
def writeShiftFile(shift_dict,output,refname):
    """ Create output shiftfile in PyDrizzle-compatible format.
        No PyDrizzle function exists, so it will work independently.
    """
    
    preface_str = "# frame: output\n"
    preface_str += "# refimage: "+refname+"[wcs]\n"
    preface_str += "# form: delta \n"
    preface_str += "# units: pixels \n"
    
    removeFile(output)
    # Open output file in write-only, overwrite mode
    outfile = open(output, mode='w')
    
    # Write out the preface strings (header)
    outfile.writelines(preface_str)
    
    # Now, for each image, write out
    # filename and shifts on a separate line
    for fname in shift_dict['order']:
        xsh = str(shift_dict[fname][0])
        ysh = str(shift_dict[fname][1])
        rot = str(shift_dict[fname][2])
        scale = str(shift_dict[fname][3])

        fstr = fname+'    '+xsh+'  '+ysh+'    '+rot+'    '+scale+'\n'
        outfile.write(fstr)
        
    #Close the file
    outfile.close()
    print('#\n#Created new shiftfile: ',output,'\n#')

def update_file_headers(shift_dict,refname,verbose=False):
    """ Apply shifts directly to image headers using tweak_updatehdr
    """
    for fname in shift_dict['order']:
        if verbose:
            print('Updating header of ',fname,' with shifted WCS values.')
        updatehdr.updatewcs_with_shift(fname,refname,rot=shift_dict[fname][2],
                scale=shift_dict[fname][3],xsh=shift_dict[fname][0],ysh=shift_dict[fname][1],
                            verbose=verbose,force=True)
    
    
def setMatchPars(input_pars):
    """ Set the parameters for the IRAF tasks used 
        for the catalog matching. 
    """
    iraf.daofind.starmap = ''
    iraf.daofind.skymap = ''
    iraf.daofind.datapars = ''
    iraf.daofind.findpars = ''
    iraf.daofind.boundary = 'nearest'
    iraf.daofind.constant = 0.0
    iraf.daofind.interactive = no
    iraf.daofind.icommands = ''
    iraf.daofind.gcommands = ''
    iraf.daofind.wcsout = ')_.wcsout'
    iraf.daofind.cache = ')_.cache'
    iraf.daofind.verify = no
    iraf.daofind.update = yes
    iraf.daofind.verbose = input_pars['verbose']
    iraf.daofind.graphics = ')_.graphics'
    iraf.daofind.display = ')_.display'
    iraf.daofind.mode = 'h'
    
    # Set daofind PSET parameters based on user input values
    iraf.datapars.fwhmpsf = input_pars['fwhmpsf'] 
    iraf.datapars.sigma = input_pars['sigma']
    iraf.datapars.datamin = input_pars['datamin']
    iraf.datapars.datamax = input_pars['datamax']
    
    iraf.findpars.threshold = input_pars['threshold']
    iraf.findpars.nsigma = input_pars['nsigma']

    # Set default values for XYXYMATCH
    iraf.xyxymatch.refpoints = ''
    #iraf.xyxymatch.xin = 'INDEF'
    #iraf.xyxymatch.yin = 'INDEF'
    iraf.xyxymatch.xin = input_pars['xyxin']
    iraf.xyxymatch.yin = input_pars['xyyin']
    iraf.xyxymatch.xmag = 'INDEF'
    iraf.xyxymatch.ymag = 'INDEF'
    iraf.xyxymatch.xrotation = 0.0
    iraf.xyxymatch.yrotation = 0.0
    iraf.xyxymatch.xref = 0.0
    iraf.xyxymatch.yref = 0.0
    iraf.xyxymatch.xcolumn = input_pars['xcol']
    iraf.xyxymatch.ycolumn = input_pars['ycol']
    iraf.xyxymatch.xrcolumn = 1
    iraf.xyxymatch.yrcolumn = 2
    iraf.xyxymatch.separation = input_pars['separation']
    iraf.xyxymatch.nmatch = input_pars['nmatch']
    iraf.xyxymatch.matching = input_pars['matching']
    iraf.xyxymatch.ratio = 10.0
    iraf.xyxymatch.nreject = 10
    iraf.xyxymatch.xformat = '%13.3f'
    iraf.xyxymatch.yformat = '%13.3f'
    iraf.xyxymatch.interactive = no
    iraf.xyxymatch.verbose = input_pars['verbose']
    iraf.xyxymatch.icommands = ''
    iraf.xyxymatch.mode = 'h'
    
    # Set default values for GEOMAP
    iraf.geomap.transforms = ''
    iraf.geomap.results = ''
    iraf.geomap.fitgeometry = input_pars['fitgeometry']
    iraf.geomap.function = input_pars['function']
    iraf.geomap.xxorder = 2
    iraf.geomap.xyorder = 2
    iraf.geomap.xxterms = 'half'
    iraf.geomap.yxorder = 2
    iraf.geomap.yyorder = 2
    iraf.geomap.yxterms = 'half'
    iraf.geomap.maxiter = input_pars['maxiter']
    iraf.geomap.reject = input_pars['reject']
    iraf.geomap.calctype = 'real'
    iraf.geomap.verbose = input_pars['verbose']
    iraf.geomap.interactive = no
    iraf.geomap.graphics = 'stdgraph'
    iraf.geomap.cursor = ''
    iraf.geomap.mode = 'h'
    
def computeStddev(fname):
    """ Compute the standard deviation of all the input images."""
    iraf.imstatistics.fields='stddev'
    iraf.imstatistics.lower= 'INDEF'
    iraf.imstatistics.upper= 'INDEF'
    iraf.imstatistics.nclip= 3
    iraf.imstatistics.lsigma= 3.0
    iraf.imstatistics.usigma= 3.0
    iraf.imstatistics.binwidth=0.1
    iraf.imstatistics.format=no
    iraf.imstatistics.cache=no

    stddev =  float(iraf.imstatistics(fname,Stdout=1)[0]) 

    return stddev

def trimCoordFiles(clist,input_pars, reference,refcat=no):
    """ Sort and trim coordinate files based on flux limit. """
    # Trimming them will need to be done with a copy of the original coord list
    # to avoid problems when re-running this program on the same data.
    # So, we need to build new filenames for these trimmed files.
    if reference == no:
        fluxunits = input_pars['fluxunits']
        fluxcol = input_pars['fluxcol']
        fluxmin = input_pars['fluxmin']
        fluxmax = input_pars['fluxmax']
        nbright = input_pars['nbright']
    else:
        fluxunits = input_pars['rfluxunits']
        fluxcol = input_pars['rfluxcol']
        fluxmin = input_pars['rfluxmin']
        fluxmax = input_pars['rfluxmax']
        nbright = input_pars['refnbright']
        
    cfiles = {}
    tcfiles = {}
    if fluxunits == 'mag':
        trim_minorder = ' <= '
        trim_maxorder = ' >= '
        _ascend = yes
    else:
        trim_minorder = ' >= '
        trim_maxorder = ' <= '
        _ascend = no
        
    fluxcol = 'c'+str(fluxcol)
    trim_expr = None
    if fluxmin != None:
        trim_expr = fluxcol+trim_minorder+str(fluxmin)
    if fluxmax != None:
        _expr = fluxcol+trim_maxorder+str(fluxmax)
        if trim_expr != None:
            trim_expr += ' && '+_expr
        else:
            trim_expr = _expr

    for img in list(clist.keys()):
        cf = clist[img]
        indx = cf.rfind('.')
        cfiles[img] = cf[:indx]+'_trim'+cf[indx:]
        tcfiles[img] = cf[:indx]+'_ftrim'+cf[indx:]
        removeFile(cfiles[img])
        if trim_expr != None:
            # Now, trim off all target positions with flux beyond the fluxlimit
            iraf.tselect(cf,tcfiles[img],trim_expr)
        else:
            iraf.tcopy(cf,tcfiles[img],verbose=no)
            
        if nbright == None:
            os.rename(tcfiles[img],cfiles[img])
        else:
            iraf.tsort(tcfiles[img],fluxcol,ascend=_ascend,casesens=yes)
            iraf.tcopy(tcfiles[img]+'[r:row=1:'+str(nbright)+']',cfiles[img],verbose=no)
            removeFile(tcfiles[img])
            
    del tcfiles
            
    return cfiles

def convertImageToSFITS(fname):
    """ Convert the input filename (possibly with
        extensions already specified) into a separate
        simple FITS file for each SCI extension.
        
        If fname refers to a simple FITS file already,
        list will only contain fname. 
    """
    flist = []
    
    # Extract any extension specifications
    rootname,extn = fileutil.parseFilename(fname)
    if extn == None:
        # Check to see if file is multi-extension
        fimg = fileutil.openImage(fname)
        if len(fimg) > 1:
            # We have multiextension FITS, so write out
            # each SCI extension as a separate simple
            # FITS file.
            for hdu in fimg:
                if 'extname' in hdu.header and hdu.header['extname'] == 'SCI':
                    extname = hdu.header['extname'].lower()+str(hdu.header['extver'])
                    new_fname = rootname[:rootname.rfind('.fits')]+'_extract_'+extname+'.fits'
                    removeFile(new_fname)
                    flist.append([new_fname,fname])
                    phdu = pyfits.PrimaryHDU(header = hdu.header,data=hdu.data)
                    phdu.writeto(new_fname)
                    del phdu
        else:
            # We already have a simple FITS, just record its name
            flist.append([None,fname])
        fimg.close()
        del fimg
    else:
        # We have an image with a SCI extension specified
        split_extn = extn.split(',')
        extname = str(split_extn[0])+str(split_extn[1])
        new_fname = rootname[:rootname.rfind('.fits')]+'_extract_'+extname+'.fits'
        removeFile(new_fname)
        iraf.imcopy(fname,new_fname,verbose=no)
        flist.append([new_fname,fname])
    
    return flist
                    
def runSExtractor(filename, sdict, pscale=0.05, gain=1.0):

    # Define DAOFIND compatible format for the output
    # SEXTRACTOR catalog
    SEXTRACTOR_OUTPUT = ["X_IMAGE", "Y_IMAGE", "FLUX_BEST", "FLUXERR_BEST","FLAGS", "FWHM_IMAGE","NUMBER"]

    # SExtractor will only run on simple FITS files,
    # so each SCI extension from MEF files need 
    # to be written out to separate simple FITS outputs.
    #
    # The function will return a list of filenames for
    # the simple FITS files based on all SCI extensions
    # specified in filename.
    imglist = convertImageToSFITS(filename)
    seobjects = []
        
    for img in imglist:
        # build name of catalog written out by SExtractor
        
        if img[0] == None:
            imgname = img[1]
        else:
            imgname = img[0] 

        catname = imgname[:imgname.rfind('.fits')]+'_sextractor.cat'
        
        # Create a SExtractor instance
        s = sextractor.SExtractor()

        # Modify the SExtractor configuration
        s.config['GAIN'] = gain
        s.config['PIXEL_SCALE'] = pscale
        s.config['CATALOG_NAME'] = catname
        # Add a parameter to the parameter list
        s.config['PARAMETERS_LIST'] = SEXTRACTOR_OUTPUT
        
        if sdict['config_file'].value == '':
            # Update in-memory parameters as set by user
            for key in sdict.keys():
                if sdict[key].value != "" and key != '$nargs' and key != 'mode':
                    if key != 'phot_autoparams':
                        s.config[key.upper()] = sdict[key].value
                    else:
                        vals = []
                        for v in sdict[key].value.split(','):
                            vals.append(float(v))
                        s.config[key.upper()] = vals
            s.update_config()                    
        else:
            s.update_config()
            s.config['CONFIG_FILE'] = sdict['config_file'].value
            
        try:
            # Lauch SExtractor on a FITS file
            s.run(imgname,updateconfig=False)
            seobjects.append(s)
            
            # Removing the configuration files, the catalog and
            # the check image
            s.clean(config=True, catalog=False, check=True)
        except sextractor.SExtractorException:
            print('WARNING: Problem running the SExtractor executable.')
            if os.path.exists(catname):
                os.remove(catname)
            del s
            
        # Now that we are done with this intermediate file,
        # clean it up...
        if img[0] != None:
            removeFile(img[0])

    return seobjects
    
        
def countSExtractorObjects(SEObjectList):
    """ Count the number of objects detected by SExtractor. """
    # Read the resulting catalog [first method, whole catalog at once]
    num_obj = 0
    for obj in SEObjectList:
        num_obj += len(obj.catalog())
    return num_obj
    
def readSExtractorCatalog(secatalog):
    """ Read object catalog produced by SExtractor and
        return positions for specific chip extension.
    
    """
    coords = []
    fluxes = []
    for obj in secatalog:
        coords.append([obj['X_IMAGE'],obj['Y_IMAGE']])
        fluxes.append(obj['FLUX_BEST'])
    return coords,fluxes

def run(input,reference=None,output='shifts.txt',
        findmode='catalog',**input_dict):
    """ Control the entire shift finding operation. """

    print('Tweakshifts Version ',__version__,'\n')
        
    # Start by determining what input files we are to work with...
    flist,outfile = parseinput.parseinput(input)
    verbose=False
    if 'verbose' in input_dict and input_dict['verbose']:
        print('Computing shifts for :')
        print(flist)
        verbose=input_dict['verbose']
    # Insure that a valid filename gets setup for reference WCS
    if reference != None:
        refname = reference
        if os.path.exists(refname):
            err_str = 'Output Reference WCS "'+refname+'" already exists! Please delete.'
            raise ValueError(err_str)
    else:
        refname = 'tweak_wcs.fits' 
    
    # split remainder of parameters into algorithm specific sets
    # and use them for depending on what algorithm was selected.
    if findmode == 'catalog':
        match_pars = parseInputPars(input_dict,MATCH_PARS)
        match_pars['input'] = input
        shift_dict = runCatalog(flist,refname,match_pars)   
    else:
        # Insure that flist does not include the name of the 
        # reference WCS as well. 
        ref_img = input_dict['crossref']
        if flist.count(ref_img) > 0: flist.remove(ref_img)

        # Refname can not be derived from 'refimage' parameter
        # as the refimage parameter serves as an output, not an
        # input, as needed here... WJH 11 May 2005
        crosscor_pars = parseInputPars(input_dict,CROSSCORR_PARS)
        shift_dict = runCrosscorr(flist,ref_img,crosscor_pars)
        w = wcsutil.WCSObject(ref_img)
        w.createReferenceWCS(refname)
    
    if len(shift_dict['order']) > 1:
        writeShiftFile(shift_dict,output,refname)
        if 'updatehdr' in input_dict and input_dict['updatehdr']:
            update_file_headers(shift_dict,refname,verbose=verbose)
    else:
        print('No suitable data for finding shifts.')
        print('No shift file written out.')
        if os.path.exists(refname):
            os.remove(refname)   
    
def runCatalog(flist,reference,input_pars):
    """ Use daofind, xyxymatch, and geomap to find shifts. 
        Inputs:
            flist      -- list of input files to compute shifts for
            reference  -- filename of reference image
            input_pars -- dictionary of input parameters 
    """
    shift_dict = {}
    if len(flist) == 0:
        return shift_dict
    
    # Define parameter values for catalog matching tasks, including
    # daofind, xyxymatch, and geomap
    setMatchPars(input_pars)
    if input_pars['gencatalog'] == 'sextractor' and sextractor != None:
        use_SExtractor = True
        sdict = iraf.sextractpars.getParDict()
    else:
        use_SExtractor = False

    refname = flist[0]

    # Check to see if user supplied their own target catalogs for
    # each input
    if input_pars['catfile'] != None:
        # Read in the names of the coord files from the input file
        coordnames = readCatFile(input_pars['catfile'])
        # Setup the name of the reference image coord file
        #
        # reference file must be one of those specified in catfile here...
        # This needs to be fixed to allow for separate reference image to be
        # given, specifically a single-drizzle image with flt.fits as inputs.
        #
        ref_coord_name = coordnames[refname] 

        # Write out referenceWCS FITS file based on reference image WCS
        #
        # ASSUMPTION: This takes the first input image as the reference image
        #           which all the coordinates were derived from.
        #           
        refwcs = wcsutil.WCSObject(refname)
        refwcs.createReferenceWCS(reference,overwrite=no)
        #del refwcs
        
    else:
        # Keep track of SExtractor objects for later access to their catalogs
        secatlist = []
        
        if input_pars['undistort'] == no:
            # Already provided with undistorted input images to extract
            # target positions from using DAOFind.
            coordnames = {}
            # We need to use 'daofind' on inputs and build
            # up coordinates files here...
            #
            print('Creating reference WCS:"',reference,'" from undistorted input:',refname)
            
            # Write out referenceWCS FITS file based on reference image WCS
            refwcs = wcsutil.WCSObject(refname)
            refwcs.createReferenceWCS(reference,overwrite=no)
            ref_pscale = refwcs.pscale
            #del refwcs
            
            # Define coordnames dictionary for input files
            coordnames[refname] = ref_coord_name = refname[:refname.rfind('.')]+'.coord'
            # If this file was left behind from a previous run, delete it.
            removeFile(ref_coord_name)

            # Place check here for user parameter to use SExtractor as
            # first choice catalog generator.
            if use_SExtractor:
                # First, try running SExtractor to measure objects from images
                _exptime = pyfits.getval(refname,'exptime')
                print('Running SExtractor on ' ,refname)
                seobjects = runSExtractor(refname,sdict,pscale=ref_pscale,gain=_exptime)
                
                if len(seobjects) == 0: 
                    use_SExtractor = False
                    print('Defaulting to using IRAF daofind for catalog creation.')
                
            if not use_SExtractor:
                # if SExtractor not available, then default to using DAOFIND
                if input_pars['computesig'] == 'yes':
                    iraf.datapars.sigma = computeStddev(refname) * 1.5
                #
                # Run daofind for REFERENCE image target positions
                #
                print('#\n# Running DAOFIND on reference image: ',refname,'\n#')
                iraf.daofind(image=refname,output=ref_coord_name)

                num_obj = countObj(ref_coord_name)
            else:
                num_obj = countSExtractorObjects(seobjects)
                # Only set these values if using SExtractor was successful
                secatlist.append(seobjects)
                # Reset name of coordinate file to that generated by SExtractor
                coordnames[refname] = ref_coord_name = seobjects[0].config['CATALOG_NAME']
                
            # Report the number of objects detected     
            print('  Found ',num_obj,' objects in reference image: ',refname)

            # Create coord files for input images
            for fname in flist:
                if fname != refname:
                    if use_SExtractor:
                        _exptime = pyfits.getval(fname,'exptime')
                        print('Running SExtractor on: ',fname)
                        seobjects = runSExtractor(fname,sdict,pscale = ref_pscale,gain=_exptime)
                        num_obj = countSExtractorObjects(seobjects)
                        secatlist.append(seobjects)
                        coordnames[fname] = fcoord_name = seobjects[0].config['CATALOG_NAME']
                    else:
                        coordnames[fname] = fcoord_name = buildName(fname)
                        # Default to using DAOFIND
                        if input_pars['computesig'] == 'yes':
                            iraf.datapars.sigma = computeStddev(fname) * 1.5
                        # Find targets in image
                        print('#\n# Running DAOFIND on input image: ',fname,'\n#')
                        iraf.daofind(image=fname,output=fcoord_name)

                        # Verify that we have identified enough stars for fit
                        num_obj = countObj(fcoord_name)

                    if num_obj >= input_pars['minobj']:
                        print('  Found ',num_obj,' objects in ',fname)
                    else: 
                        print('#########\n#  Not enough objects found in ',fname,' for matching.')
                        print('#  Skipping this image.\n#########')
                        continue
        else:
            # Input images are assumed to be distorted inputs (FLT/CRJ/SFL) files
            # so, positions extracted from the input images should be undistorted
            # using PyDrizzle methods.
            #
            # Start by making sure that the specified reference image is 
            # in the list of filenames used to build the PyDrizzle object
            # In fact, it has to be the first one...
            _indx = input_pars['input'].find('_asn.fits')
            if  _indx > -1:
                asnroot = input_pars['input'][:_indx]
            else:
                asnroot = 'tweak_match'
                asnid = buildNewAsnTable(flist, refname, asnroot)
                
            #if input_pars['shiftfile'] != None:
            #    updateasn.updateShifts(asnroot+'_asn.fits',input_pars['shiftfile'],mode='sum')

            # Build PyDrizzle object for this association
            #pyasn = pydrizzle.PyDrizzle(flist,bits_single=None,bits_final=None,idckey=input_pars['idckey'])
            pyasn = pydrizzle.PyDrizzle(input_pars['input'],output=asnroot,bits_single=None,bits_final=None,
                                        prodonly=False,idckey=input_pars['idckey'],
                                        shiftfile=input_pars['shiftfile'])
            
            # Reference WCS will be output product WCS
            refwcs = pyasn.observation.product.getWCS()
            
            # Write out undistorted WCS to a simple FITS file for use as
            # a reference image by later calls to PyDrizzle/MultiDrizzle
            refwcs.createReferenceWCS(reference,overwrite=no)        
            
            # For each member chip, find targets using DAOFind, then 
            # undistort using PyDrizzle method 'wtraxy' and concatenate results
            # for each file.
            coordnames = {}
            memnames = pyasn.observation.getMemberNames()
            pyasn_ref_coord_name = buildName(list(memnames[0].keys())[0])
            ref_coord_name = buildName(flist[0])

            for mem in memnames: 
                fname = list(mem.keys())[0]
                
                coordnames[fname] = coordname = buildName(fname)    
                cxylist = []
                fluxes = []
                # for each file, loop over member chips to extract targets
                for chip in mem[fname]:                
                    member = pyasn.getMember(chip)
                    # Build name of temporary coord file for the raw positions 
                    # from this chip
                    fcoord_name = buildName(chip,keepext=yes)
                    removeFile(fcoord_name)

                    if use_SExtractor:
                        # First, try running SExtractor to measure objects from images
                        seobjects = runSExtractor(chip,sdict,pscale=member.pscale)
                        if len(seobjects) == 0: 
                            use_SExtractor = False
                                                                         
                    if not use_SExtractor:
                        # Default to DAOFIND, or use daofind as specified by user
                        if input_pars['computesig'] == 'yes':
                            iraf.datapars.sigma = computeStddev(chip) * 1.5
                        # Find targets in chip
                        print('#\n# Running DAOFIND on distorted image: ',chip,'\n#')
                        iraf.daofind(image=chip,output=fcoord_name)

                        # Read in raw positions from coord catalog
                        coordlist,fluxlist = readCoordCatalog(fcoord_name,fluxcol=input_pars['fluxcol'])
                    else:
                        # We have used SExtractor to generate a catalog
                        # Now, read those positions and convert them to
                        # undistorted positions relative to the reference WCS
                        coordlist,fluxlist = readSExtractorCatalog(seobjects[0].catalog())

                    print('    Computing undistorted target positions for ',len(coordlist),'objects.')
                    # for each raw position, find undistorted position
                    cxylist.extend(member.geometry.wtraxy(coordlist,refwcs))
                                    
                    fluxes.extend(fluxlist)                   
                    if input_pars['clean'] == yes:
                        removeFile(fcoord_name)
                        if use_SExtractor:
                            removeFile(seobjects[0].config['CATALOG_NAME'])
                    
                # Write out combined list of undistorted positions for this file
                writeCoordFile(cxylist, fluxes, coordname)
                        
    #
    # Match coordinate catalog targets and perform fits
    #
    # Start by sorting and trimming catalogs based on flux values
    if input_pars['fluxmax'] != None or input_pars['fluxmin'] != None or input_pars['nbright'] != None:
        if input_pars['verbose'] == yes:
            print('#\n# Trimming coordinate files \n#')
            if input_pars['nbright'] != None:
                print('     Catalog trimmed to ',input_pars['nbright'],'brightest with fluxlimits of ',input_pars['fluxmax'],'-',input_pars['fluxmin'])
            else:
                print('     Catalog trimmed to fluxlimits of ',input_pars['fluxmax'],'-',input_pars['fluxmin'])
        coordfiles = trimCoordFiles(coordnames,input_pars ,reference)
        ref_coord_name = coordfiles[refname]
    else:
        coordfiles = coordnames

    # Now that we have coordinate catalogs for each input, 
    # check to see whether we have a reference catalog.
    # If not, perform fit as normal, otherwise, fit against
    # the reference catalog after read in and converted
    # to the input WCS frame (since all matching gets performed
    # in X/Y space).
    if input_pars['refcat'] != None:
        refname = input_pars['refcat']
        ref_coord_name = refname[:refname.rfind('.')]+'_tweak_ref.coord'
        shift_dict['order'] = []
        
        # Read in reference catalog
        rtarglist,rfluxlist = readCoordCatalog(refname,xcol=input_pars['refxcol'],\
                                ycol=input_pars['refycol'],fluxcol=input_pars['rfluxcol'])
 
        # Convert RA/Dec positions to reference WCS X/Y positions

        refxylist = []
        for ii in rtarglist:
             refxylist_ii = refwcs.rd2xy(ii)
             refxylist.append(refxylist_ii)

        # Write out combined list of undistorted positions for this file
        writeCoordFile(refxylist, rfluxlist, ref_coord_name)
        
        rcoordnames = {refname:ref_coord_name}
        # Perform any desired trimming of reference coordinate catalog
        if input_pars['rfluxmax'] != None or input_pars['rfluxmin'] != None or input_pars['refnbright'] != None:
            rcoordfiles = trimCoordFiles(rcoordnames,input_pars ,reference,refcat=yes)
        else:
            rcoordfiles = rcoordnames
        
    else:
        print('Refname: ',refname)
        outname = define_output_name(refname)
        shift_dict['order'] = [outname]
        shift_dict[outname] = (0.,0.,0.,1.0)
            

    print('#\n# Matching detected objects \n#')
    print('    Reference image: ',ref_coord_name)
    
    for fname in flist:
        if fname != refname:
            # Define rootname for intermediate products
            fname_root = fname[:fname.rfind('.')]
            
            # Define output filenames for intermediate files
            fcoord_name = coordfiles[fname]
            fcoord_match = fname_root+'.match'
            fcoord_db = fname_root+'.db'
            # If any of these still exist from a previous run, 
            # delete them before starting again...
            removeFile(fcoord_match)    
            removeFile(fcoord_db)            

            print('    Input          : ',fcoord_name) 
            # Perform XYXYmatch on images
            iraf.xyxymatch(input=fcoord_name, reference=ref_coord_name,
                output=fcoord_match,tolerance=input_pars['tolerance'])

            num_obj = countObj(fcoord_match)
            print('num_obj: ',num_obj,'  minobj : ',input_pars['minobj'])
            if num_obj >= input_pars['minobj']:
                print('  Fitting for shifts using ',num_obj,' matched objects.')
            else:
                print('#########\n#  Not enough matched objects for fit.')
                print('#  Skipping this image.\n#########')
                continue

            # shift coordinates zero-point from corner of image to the center
            # so that rotation and shift computed by geomap will be 
            # appropriate for 'drizzle'
            ref_cenx = str(refwcs.naxis1 / 2.0)
            ref_ceny = str(refwcs.naxis2 / 2.0)
            iraf.tcalc(table=fcoord_match,outcol='c1',equals='c1-'+ref_cenx)
            iraf.tcalc(table=fcoord_match,outcol='c2',equals='c2-'+ref_ceny)
            iraf.tcalc(table=fcoord_match,outcol='c3',equals='c3-'+ref_cenx)
            iraf.tcalc(table=fcoord_match,outcol='c4',equals='c4-'+ref_ceny)

            
            print('#\n# Performing fit to determine shift. \n#')
            print('GEOMAP input: ',fcoord_match)
            # Perform fitting on matched list using GEOMAP
            iraf.geomap(input=fcoord_match,database=fcoord_db,
                        xmin='INDEF',xmax='INDEF',ymin='INDEF',ymax='INDEF')

            # parse out the results from the GEOMAP database file
            outname = define_output_name(fname)
            shift_dict['order'].append(outname)
            shift_dict[outname] = readDBFile(fcoord_db)
            if input_pars['clean'] == yes:
                # Remove image specific intermediate files; namely,
                #    matched list of objects from XYXYMATCH
                removeFile(fcoord_match)
                # and database file from GEOMAP
                removeFile(fcoord_db)
                
    # Remove intermediate files
    if input_pars['clean'] == yes:
        for img in coordfiles.keys():
            removeFile(coordfiles[img])
            removeFile(coordnames[img])
            # ref_coord_name may have come from external catalog
            # so it will not be part of coordnames dict
            removeFile(ref_coord_name)
        
    return shift_dict
    
def runCrosscorr(flist,reference,input_pars):
    """ Use cross-correlation to compute shifts.
        Input and reference will ONLY be singly-drizzled images.
    """
    shift_dict = {}
    
    iraf.set(imtype='fits')
        
    # Run 'crossdriz' with proper inputs
    iraf.unlearn('crossdriz')
    iraf.unlearn('shiftfind')

    iraf.crossdriz.dinp = input_pars['dinp']
    iraf.crossdriz.dref = input_pars['dref']
    iraf.crossdriz.pad = input_pars['pad']
    iraf.crossdriz.margin = input_pars['margin']
    iraf.crossdriz.tapersz = input_pars['tapersz']
    iraf.crossdriz.mintheta = 0.0
    iraf.crossdriz.maxtheta = 0.0
    iraf.crossdriz.stptheta = 0.1

    iraf.shiftfind.xshift = 0.
    iraf.shiftfind.xshift = 0.
    iraf.shiftfind.boxsize = 'INDEF'
    iraf.shiftfind.fwhm = input_pars['xc_fwhm']
    iraf.shiftfind.ellip = input_pars['xc_ellip']
    iraf.shiftfind.pa = input_pars['xc_pa']
    iraf.shiftfind.fitbox = input_pars['xc_fitbox']
    iraf.shiftfind.kellip = yes
    iraf.shiftfind.kpa = yes
    iraf.shiftfind.tempdir = "tmp$"
    
    
    print('#\n# Performing Cross-correlation with CROSSDRIZ \n#')
    print('    Reference image  : ',reference)
    xcorr_root = 'tweak_xcorr'
    xcorr_sroot = 'xcorr_shifts'
    
    outname = define_output_name(reference)
    shift_dict['order'] = [outname]
    shift_dict[outname] = [0.,0.,0.,1.0]

    indx = 0
    for img in flist:
        print('    Single input     : ',img)
        xcorr_indx = "%03d"%indx
        xcorr_base = xcorr_root + xcorr_indx
        xcorr_shift = xcorr_sroot + xcorr_indx + '.txt'
        
        # Insure that products from previous runs are deleted
        xcorr_list = irafglob.irafglob(xcorr_base+'*')
        for file in xcorr_list: removeFile(file)

        iraf.crossdriz(image=img, refimage=reference, basename = xcorr_base)        

        # Now, run shiftfind to compute the shifts 
        removeFile(xcorr_shift)
        print('#\n# Computing shift using SHIFTFIND \n#')
        iraf.shiftfind(cclist=xcorr_base,outfile=xcorr_shift)

        # Read shifts from shiftfind file...
        xcfile = open(xcorr_shift,'r')
        xclines = xcfile.readlines()
        xcol = 2
        ycol = 4
        for xcl in xclines:
            if xcl[0] == '#' or xcl.strip() == '': continue

            xcl = xcl.split()
            outname = define_output_name(xcl[0])
            shift_dict[outname] = (-float(xcl[xcol]),-float(xcl[ycol]),0.0,1.0)

        xcfile.close()
        
        # Clean up intermediate CROSSDRIZ products
        if input_pars['clean'] == yes:
            removeFile(xcorr_shift)
            xcorr_list = irafglob.irafglob(xcorr_base+'*')
            for file in xcorr_list: removeFile(file)
        # Used for indexing xcorr_base rootname for all inputs
        indx += 1
        if img != reference:
            outname = define_output_name(img)
            shift_dict['order'].append(outname)
    

    return shift_dict
