from __future__ import print_function

import os
from astropy.io import fits as pyfits
import numpy as np

from stsci.tools import fileutil
import stwcs
from stwcs import wcsutil,updatewcs

wcs_keys = ['CRVAL1','CRVAL2','CD1_1','CD1_2','CD2_1','CD2_2','CRPIX1','CRPIX2','ORIENTAT']
prihdr_keys = ['PA_V3'] # WCS keywords found in PRIMARY header
pad_rows = 8

def update_from_shiftfile(shiftfile,force=False):
    """ Update headers of images specified in shiftfile with shifts from shiftfile.
    """
    f = open(shiftfile)
    shift_lines = f.readlines()
    f.close()

    # interpret header of shift file
    for line in shift_lines:
        if 'refimage' in line:
            refimage = line.split(':')[-1]
            refimage = refimage[:refimage.find('[wcs]')]
            break
    
    # Now read in numerical values from shiftfile
    type_list = {'names':('fnames','xsh','ysh','rot','scale'),
                 'formats':('S24','f4','f4','f4','f4')}
    sdict = np.loadtxt(shiftfile,dtype=type_list,unpack=True)
    for img in sdict:
        updatewcs_with_shift(img['fnames'],refimage,
                rot=img['rot'],scale=img['scale'],
                xsh=img['xsh'],ysh=img['ysh'],force=force)

def updatewcs_with_shift(image,reference,rot=0.0,scale=1.0,xsh=0.0,ysh=0.0,
                            verbose=False,force=False):
    """ Update the SCI headers in 'image' based on the fit provided as determined
        in the WCS specified by 'reference'.  The fit should be a 2-D matrix as 
        generated for use with 'make_vector_plot()'.
        
        Algorithm
        ----------
        Apply the following steps to the input image's reference chip WCS:
            1- compute RA/Dec with full distortion correction for 
                reference point as (Rc_i,Dc_i)
            2- find the Xc,Yc for each Rc_i,Dc_i and get the difference from the
                CRPIX position for the reference WCS as (dXc_i,dYc_i)
            3- apply fit (rot&scale) to (dXc_i,dYc_i) then apply shift, then add 
                CRPIX back to get new (Xcs_i,Ycs_i) position
            4- compute (Rcs_i,Dcs_i) as the sky coordinates for (Xcs_i,Ycs_i)
            5- compute delta of (Rcs_i-Rc_i, Dcs_i-Dcs_i) as (dRcs_i,dDcs_i) 
            6- apply the fit to the chip's undistorted CD matrix, the apply linear
                distortion terms back in to create a new CD matrix
            7- add (dRcs_i,dDcs_i) to CRVAL of the reference chip's WCS 
            8- update header with new WCS values
            9- run updatewcs to update all chips to be consistent with 
                input images's updated reference chip's WCS
        TODO:
            - Work out how to allow headerlet WCS's as input for 'reference'
            [The solution would probably be to recognize 'WCSCORR' as the EXTNAME
            of the headerlet and interpret it much like the shiftfile ref_wcs
            FITS file implemented here. The shift,rot and scale values would then
            come directly from the WCSCORR extension, which itself would be
            appended to the input image as a record of what correction was done.]
            
    """
    print('Updating header for ',image)
    #restore_wcs_file(image,wcsprefix='O')
    # Create initial WCSCORR extension and archive WCS keyword values, if needed
    init_wcscorr(image,force=force)
    # reset header WCS keywords to original (OPUS generated) values
    restore_file_from_wcscorr(image)

    # if input reference is a ref_wcs file from tweakshifts, use it
    refimg = pyfits.open(reference)
    wref = None
    for extn in refimg:
        if 'extname' in extn.header and extn.header['extname'] == 'WCS':
            wref = wcsutil.WCS(refimg['wcs'].header)
            break
    refimg.close()
    # else, we have presumably been provided a full undistorted image 
    # as a reference, so use it with HSTWCS instead
    if wref is None:
        wref = wcsutil.HSTWCS(reference)

    # compute the matrix for the scale and rotation correction
    fit = scale*fileutil.buildRotMatrix(rot)
    
    # archive and update PA_V3
    fimg= pyfits.open(image,mode='update')

    #fimg[0].header.update('HPA_V3',fimg[0].header['PA_V3'])
    fimg[0].header['PA_V3'] += rot
    fimg.close()

    # for each chip in image, apply algorithm
    fimg = pyfits.open(image)
    nchip,extn = updatewcs.getNrefchip(fimg)
    extver = fimg[extn].header['extver']
    fimg.close()

    if verbose:
        print('Processing SCI,',extver)
    chip_wcs = wcsutil.HSTWCS(image,('sci',extver))
    
    # step 1
    xpix = [chip_wcs.wcs.crpix[0],chip_wcs.wcs.crpix[0]+1,chip_wcs.wcs.crpix[0]]
    ypix = [chip_wcs.wcs.crpix[1],chip_wcs.wcs.crpix[1],chip_wcs.wcs.crpix[1]+1]

    # This full transformation includes all parts of model, including DGEO
    Rc_i,Dc_i = chip_wcs.all_pix2sky(xpix,ypix,1)    
    
    # step 2
    Xc_i,Yc_i = wref.wcs_sky2pix([Rc_i],[Dc_i],1)
    Xc_i -= wref.wcs.crpix[0]#+xsh
    Yc_i -= wref.wcs.crpix[1]#+ysh
    # step 3
    Xcs_i,Ycs_i = apply_db_fit([Xc_i,Yc_i],fit,xsh=-1*xsh,ysh=-1*ysh)
    Xcs_i += wref.wcs.crpix[0]#+xsh
    Ycs_i += wref.wcs.crpix[1]#+ysh

    chip_fit = fit
    # step 4
    Rcs_i,Dcs_i = wref.wcs_pix2sky(Xcs_i,Ycs_i,1)            
    # step 5
    # new crval should be first member
    chip_wcs.wcs.crval = np.array([Rcs_i[0],Dcs_i[0]])
    new_crval1 = Rcs_i[0]
    new_crval2 = Dcs_i[0]
    # step 6
    # see about computing the CD matrix directly from the 3 points around
    # the shifted/rotated CRPIX position in the output frame as projected
    # back onto the sky
    am1 = Rcs_i[1]-new_crval1
    bm1 = Rcs_i[2]-new_crval1
    cm1 = Dcs_i[1]-new_crval2
    dm1 = Dcs_i[2]-new_crval2
    new_cdmat = chip_wcs.wcs.cd = np.array([[am1*np.cos(new_crval2*np.pi/180),bm1*np.cos(new_crval2*np.pi/180)],[cm1,dm1]],dtype=np.float64)

    # step 8    
    update_wcs(image,extver,chip_wcs,wcsprefix='O',verbose=verbose)
    updatewcs.updatewcs(image,checkfiles=False)
    
    # Record updated values in WCSCORR extension now
    archive_wcs_file(image)

   
###
### WCSEXT table related keyword archive functions
### 
def init_wcscorr(input,force=False):
    """ This function will initialize the WCSCORR table if it is not already present,
        and look for WCS keywords with a prefix of 'O' as the original OPUS
        generated WCS as the initial row for the table or use the current WCS
        keywords as initial row if no 'O' prefix keywords are found. 
        
        This function will NOT overwrite any rows already present.
        
        This function works on all SCI extensions at one time.
    """
    if isinstance(input,str):
        # input must be a filename, so open as PyFITS object
        fimg = pyfits.open(input,mode='update')
        need_to_close = True
    else:
        fimg = input
        need_to_close = False

    # Verify that a WCSCORR extension does not already exist...
    for extn in fimg:
        if 'extname' in extn.header and extn.header['extname'] == 'WCSCORR':
            if not force:
                return
            else:
                del fimg['WCSCORR']
    # define the primary columns of the WCSEXT table with initial rows for each
    # SCI extension for the original OPUS solution
    numsci = count_ext(fimg)
    # create new table with more rows than needed initially to make it easier to
    # add new rows later
    wcsext = create_wcscorr(numrows=numsci,padding=numsci*4)
    # Assign the correct EXTNAME value to this table extension
    wcsext.header.update('TROWS',numsci*2,comment='Number of updated rows in table')
    wcsext.header.update('EXTNAME','WCSCORR',comment='Table with WCS Update history')

    # Now copy original OPUS values into table
    for extver in range(1,numsci+1):
        rowind = find_wcscorr_row(wcsext.data,{'wcs_id':'OPUS','extver':extver})
        # There should only EVER be a single row for each extension with OPUS values
        rownum = np.where(rowind)[0][0]
        #print 'Archiving OPUS WCS in row number ',rownum,' in WCSCORR table for SCI,',extver

        hdr = fimg['SCI',extver].header
        # Look for old-style archived keywords for OPUS-generated values
        prefix = 'O'
        if 'O'+wcs_keys[0] not in hdr:
            # No such keywords, so turn off this prefix and use primary WCS keywords
            prefix = ''
        if wcsext.data.field('CRVAL1')[rownum] != 0:
            # If we find values for these keywords already in the table, do not
            # overwrite them again
            print('WCS keywords already updated...')
            break   
        for key in wcs_keys:
            wcsext.data.field(key)[rownum] = hdr[(prefix+key)[:8]]
        # Now get any keywords from PRIMARY header needed for WCS updates
        for key in prihdr_keys:
            wcsext.data.field(key)[rownum] = fimg[0].header[key]
            
    
    # Now copy current WCS values into table
    for extver in range(1,numsci+1):
        hdr = fimg['SCI',extver].header

        # identify next empty row
        rowind = find_wcscorr_row(wcsext.data,selections={'wcs_id':' '})
        rows = np.where(rowind)
        if len(rows[0]) > 0:
            rownum = np.where(rowind)[0][0]
        else:
            print('No available rows found for updating. ')
        #print 'Archiving current WCS row number ',rownum,' in WCSCORR table for SCI,',extver
        
        # Update selection columns for this row with relevant values
        idctab =fileutil.osfn(fimg[0].header['idctab'])
        idcname = os.path.split(idctab)[-1]
        idcname = idcname[:idcname.find('_')]
        wcsext.data.field('WCS_ID')[rownum] = 'IDC_'+idcname
        wcsext.data.field('EXTVER')[rownum] = extver

        # Look for standard WCS keyword values
        for key in wcs_keys:            
            wcsext.data.field(key)[rownum] = hdr[key]
        # Now get any keywords from PRIMARY header needed for WCS updates
        for key in prihdr_keys:
            wcsext.data.field(key)[rownum] = fimg[0].header[key]
                
    # Append this table to the image FITS file
    fimg.append(wcsext)
    # force an update now
    # set the verify flag to 'warn' so that it will always succeed, but still
    # tell the user if PyFITS detects any problems with the file as a whole
    fimg.flush('warn')
    
    if need_to_close:
        fimg.close()

def find_wcscorr_row(wcstab, selections):
    """ Return an array of indices from the table (NOT HDU) 'wcstab' that matches the 
        selections specified by the user. 
        
        The row selection criteria must be specified as a dictionary with 
        column name as key and value(s) representing the valid desired row values.
        For example, {'wcs_id':'OPUS','extver':2}.
    """
    mask = None
    for i in selections:
        bmask = (wcstab.field(i) == selections[i])
        if mask is None:
            mask = bmask.copy()
        else:
            mask = np.logical_and(mask,bmask)
        del bmask
    return mask

def archive_wcs_file(image):
    """ Update WCSCORR table with rows for each SCI extension to record the 
        newly updated WCS keyword values.
    """
    fimg = pyfits.open(image,mode='update')
    numsci = count_ext(fimg)
    for extn in range(1,numsci+1):
        update_wcscorr(fimg,fimg['sci',extn].header)
    fimg.close()

def update_wcscorr(fimg,hdr,selections=None):
    """ Update WCSCORR table with a new row for this extension header. It simply
    copies the current set of WCS keywords as a new row of the table.  
    """
    if selections is None:
        # define the WCS ID for this update
        selections = {'WCS_ID':'TWEAK_'+fileutil.getDate(),'EXTVER':hdr['extver']}
        
    # create new table for hdr and populate it with the newly updated values
    new_table = create_wcscorr()
    # ===> NOTE: need to add checks to insure that these IDs are unique
    # Update selection column values
    for key in selections:
        new_table.data.field(key)[0] = selections[key]

    for key in wcs_keys:
        new_table.data.field(key)[0] = hdr[key]

    for key in prihdr_keys:
        new_table.data.field(key)[0] = fimg[0].header[key]
            
    # Now, we need to merge this into the existing table
    old_table = fimg['WCSCORR']
    rowind = find_wcscorr_row(old_table.data,{'wcs_id':' '})
    old_nrows = np.where(rowind)[0][0]

    # check to see if there is room for the new row
    if (old_nrows + 1) > old_table.data.shape[0]:
        # if not, create a new table with 'pad_rows' new empty rows
        upd_table = pyfits.new_table(old_table.columns,nrows=old_table.data.shape[0]+pad_rows)
    else:
        upd_table = old_table
    # Now, add 
    for name in old_table.columns.names:
        upd_table.data.field(name)[old_nrows:old_nrows+1] = new_table.data.field(name)
    upd_table.header.update('TROWS',old_nrows+1)

    # replace old extension with newly updated table extension
    fimg['WCSCORR'] = upd_table
    
    
    
def restore_file_from_wcscorr(image,id='OPUS'):
    """ Copies the values of the WCS from the WCSCORR based on ID specified by user.
    The default will be to restore the original OPUS-derived values.
    """
    fimg = pyfits.open(image,mode='update')
    numsci = count_ext(fimg)
    wcs_table = fimg['WCSCORR']
    orig_rows = (wcs_table.data.field('WCS_ID') == 'OPUS')
    for extn in range(1,numsci+1):
        # find corresponding row from table
        ext_rows = (wcs_table.data.field('EXTVER') == extn)
        erow = np.where(np.logical_and(ext_rows,orig_rows))[0][0]
        for key in wcs_keys:
            fimg['sci',extn].header[key] = wcs_table.data.field(key)[erow]
        for key in prihdr_keys:
            fimg[0].header[key] = wcs_table.data.field(key)[erow]

    # close the image now that the update has been completed.
    fimg.close()
    
def create_wcscorr(descrip=False, numrows=1, padding=0):
    """ Return the basic definitions for a WCSCORR table.
    The dtype definitions for the string columns are set to the maximum allowed so 
    that all new elements will have the same max size which will be automatically
    truncated to this limit upon updating (if needed).

    The table is initialized with rows corresponding to the OPUS solution
    for all the 'SCI' extensions.
    """ 
    trows = numrows + padding
    c1 = pyfits.Column(name='WCS_ID',format='24A',array=np.array(['OPUS']*numrows+['']*padding,dtype="S24"))
    c2 = pyfits.Column(name='EXTVER',format='I',array=np.array(list(range(1,numrows+1)),dtype=np.int16))
    c3 = pyfits.Column(name='CRVAL1',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c4 = pyfits.Column(name='CRVAL2',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c5 = pyfits.Column(name='CD1_1',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c6 = pyfits.Column(name='CD1_2',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c7 = pyfits.Column(name='CD2_1',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c8 = pyfits.Column(name='CD2_2',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c9 = pyfits.Column(name='ORIENTAT',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c10 = pyfits.Column(name='PA_V3',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c11 = pyfits.Column(name='Delta_RA',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c12 = pyfits.Column(name='Delta_Dec',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c13 = pyfits.Column(name='RMS_RA',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c14 = pyfits.Column(name='RMS_Dec',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c15 = pyfits.Column(name='Delta_Orientat',format='D',array=np.array([0.0]*trows,dtype=np.float64))
    c16 = pyfits.Column(name='Delta_Scale',format='D',array=np.array([1.0]*trows,dtype=np.float64))
    c17 = pyfits.Column(name='NMatch',format='J',array=np.array([0]*trows,dtype=np.int32))
    c18 = pyfits.Column(name='Catalog',format='40A',array=np.array([''],dtype="S40"))
    if descrip:
        c19 = pyfits.Column(name='Descrip',format='128A',array=np.array(['Original WCS computed by OPUS']*numrows,dtype="S128"))
        cdefs = pyfits.ColDefs([c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19])
    else:
        cdefs = pyfits.ColDefs([c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18])

    return pyfits.new_table(cdefs,nrows=trows)

def count_ext(fimg,extname='SCI'):
    """ Return the number of 'extname' extensions, defaulting to counting the
    number of SCI extensions.
    """
    n = 0
    for e in fimg:
        if 'extname' in e.header and e.header['extname'] == extname:
            n += 1
    return n

def apply_db_fit(data,fit,xsh=0.0,ysh=0.0):
    xy1x = data[0]
    xy1y = data[1]
    numpts = xy1x.shape[0]
    if fit is not None:
        xy1 = np.zeros((xy1x.shape[0],2),np.float64)
        xy1[:,0] = xy1x 
        xy1[:,1] = xy1y 
        xy1 = np.dot(xy1,fit)
        xy1x = xy1[:,0] + xsh
        xy1y = xy1[:,1] + ysh
    return xy1x,xy1y

###
### Header keyword prefix related archive functions
###
def update_wcs(image,extver,new_wcs,extname='SCI',wcsprefix="",verbose=False):
    """ Updates the WCS of the specified (extname,extver) with the new WCS
        after archiving the original WCS.  
        
        The value of 'new_wcs' can either be the full HSTWCS object or just
        the pywcs object within the HSTWCS object.
    """
    # If an HSTWCS object is passed in for 'new_wcs', we only need the
    # pywcs object within
    if isinstance(new_wcs,wcsutil.HSTWCS):
        new_wcs = new_wcs.wcs
        
    # Open the file for updating the WCS 
    try:
        print('Updating header for ',image,'[',extname,',',extver,']')
        fimg = pyfits.open(image,mode='update')
        hdr = fimg[extname,extver].header
        
        if verbose:
            print('Updating header with new values...')
        # Insure that if a copy of the WCS has not been created yet, it will be now
        #archive_wcs(hdr,wcsprefix=wcsprefix)
        
        # update the values in the WCS 
        hdr[(wcsprefix+'CRVAL1')[:8]] = new_wcs.crval[0]
        hdr[(wcsprefix+'CRVAL2')[:8]] = new_wcs.crval[1]
        hdr[(wcsprefix+'CD1_1')[:8]] = new_wcs.cd[0][0]
        hdr[(wcsprefix+'CD1_2')[:8]] = new_wcs.cd[0][1]
        hdr[(wcsprefix+'CD2_1')[:8]] = new_wcs.cd[1][0]
        hdr[(wcsprefix+'CD2_2')[:8]] = new_wcs.cd[1][1]
        # Recompute and update ORIENTAT keyword
        orientat = fileutil.RADTODEG(np.arctan2(new_wcs.cd[0][1],new_wcs.cd[1][1]))

        # Reset DGEOEXT in reference chip header to get updatewcs to reset 
        # the DGEO extensions based on the updated WCS keywords
        # but only if we are updating the archived version of the keywords
        if wcsprefix is not '' and fimg[0].header['NPOLFILE'] not in ['',' ','N/A']:
            hdr['NPOLEXT'] = ''

    finally:
        # finish up by closing the file now
        fimg.close()
    
def archive_wcs(hdr,prefix='H',wcsprefix="",force=False):
    """ Stores a copy of relevant WCS keywords as keywords with a different
    prefix (for example, CRVAL1 -> HCRVAL1)
    It will also make the copy from OCRVAL1 instead if 'wcsprefix' is specified.
    """
    if prefix+wcs_keys[0] not in hdr or (prefix+wcs_keys[0] in hdr and force):
        for key in wcs_keys:
            hdr.update((prefix+key)[:8],hdr[(wcsprefix+key)[:8]])

def restore_wcs(hdr,prefix='H',wcsprefix=""):
    """ Restores the WCS archived with 'prefix' to the WCS keywords 
    starting with 'wcsprefix'.  By default, it will restore the "H" keywords
    as the standard WCS keywords (no prefix).
    """        
    if prefix+wcs_keys[0] in hdr:
        for key in wcs_keys:
            hdr.update((wcsprefix+key)[:8],hdr[(prefix+key)[:8]])
            
def restore_wcs_file(filename,prefix='H',wcsprefix=""):
    fimg = pyfits.open(filename,mode='update')
    if prefix+'PA_V3' in fimg[0].header:
        fimg[0].header['PA_V3'] = fimg[0].header[prefix+'PA_V3']
    nextns = count_chips(filename)
    for extn in range(1,nextns+1):
        hdr = fimg['sci',extn].header
        restore_wcs(hdr,prefix=prefix,wcsprefix=wcsprefix)
        restore_wcs(hdr,prefix='O',wcsprefix='')
    fimg.close()

