"""
$Revision: 1.10 $ $Date: 2011-06-14 16:38:56 $
Author: Martin Kuemmel
Affiliation: Space Telescope - European Coordinating Facility
"""
from __future__ import absolute_import, print_function

__author__ = "Martin Kuemmel <mkuemmel@eso.org>, Megan Sosey <sosey@stsci.edu>"
__date__ = "$Date: 2013-02-06 $"
__version__ = "$Revision: 2.0 $"
__credits__ = """This software was developed by the ACS group of the Space Telescope -
European Coordinating Facility (ST-ECF). The ST-ECF is a department jointly
run by the European Space Agency and the European Southern Observatory.
It is located at the ESO headquarters at Garching near Munich. The ST-ECF
staff supports the European astronomical community in exploiting the research
opportunities provided by the earth-orbiting Hubble Space Telescope.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
"""
"""
Howard Bushouse, STScI, 11-Feb-2011, version 1.8
Added checks in get_flambda_from_magab and get_stmag_from_magab for
wlength=0 before using it in division operation.

Howard Bushouse, STScI, 08-Mar-2011, version 1.9
Changed "raise err_msg" statements to "raise aXeError(err_msg)" to conform to
latest Python syntax rules.

Howard Bushouse, STScI, 14-Jun-2011, version 1.10
Replaced calls to simple IRAF tasks, like imcopy, imrename, imexpr, with
native python, pyfits, and numpy functions. The only remaining IRAF task
is "blot".

Megan sosey, STScI, 6-Feb-2013
Added Astrodrizzle Capability and fixed reference pixel offset for muldrizzle blot step
when excess pixels are used

Megan Sosey, STScI, 21-May-2014
Nor noticed that in fcubeprep, the images were not quite recieving the correct WCS. We traced this
to the astrodrizzle wcs, and he found that the extra SIP wcs information needed to be used in the header
reset when dim_info was not 0,0,0,0. The edits are to flt_wcs.sip
"""

from .axeerror import aXeError
import drizzlepac
from drizzlepac import astrodrizzle
from stwcs.wcsutil import HSTWCS
#from astropy import wcs as HSTWCS
from astropy.io import fits as pyfits
from stsci.tools import fileutil
import numpy as np

class MDrizzle_Image(object):
    """
    Class for multidrizzled images
    """
    def __init__(self, mdrizzle_image, ext=0):
        """
        Initialize the class
        """
        # store the image name
        self.image = mdrizzle_image

        # store the extension number
        self.ext = ext

        # determine and store the
        # number of drizzled images
        self.ndrizzle = self._get_nfcube(mdrizzle_image, ext)

    def _get_nfcube(self, image, ext):
        """
        Get the number of requested fluxcubes

        The method looks in the header of the grism image
        for the name of the input images used in the multidrizzle
        process. This number is returned

        @param image: the name of the multidrizzled grism image
        @type image: string

        @return: the number of input images
        @rtype: int
        """
        from astropy.io import fits as pyfits

        # open the fits file and get the header
        img  = pyfits.open(image, 'readonly')
        head = img[ext].header

        # create the keyname for the first input image
        ID = 1
        keyname = 'D%03iDATA' % (ID)

        # create the keyname for subsequent input images
        # and continue until the keynames do not exist
        while keyname in head:
            ID = ID+1
            keyname = 'D%03iDATA' % (ID)

        # close the grism image
        img.close()

        # correct the number
        ID = ID-1

        # return the number
        return ID

    def delete_drizzlekeys(self):
        """
        Delete all drizzle keywords

        The method deletes all MultiDrizzle related keywords.
        For each input frame there exist around 30 keywords
        describing the drizzle parameters. Due to a bug
        in the fits kernel the multitude of descriptors
        crashes the blot algorithm.
        Deleting some keywords helps.
        """
        from astropy.io import fits as pyfits

        print('Deleting drizzle descriptors in: ' + self.image + '...', end=' ')

        # open the fits file and get the header
        img  = pyfits.open(self.image, 'update')
        head = img[self.ext].header


        # list of keyord names to be deleted
        keynames = ['D%03iDATA', 'D%03iDEXP', 'D%03iOUDA', 'D%03iOUWE',
                    'D%03iOUCO', 'D%03iMASK', 'D%03iWTSC', 'D%03iKERN',
                    'D%03iPIXF', 'D%03iCOEF', 'D%03iXGIM', 'D%03iYGIM',
                    'D%03iLAM',  'D%03iSCAL', 'D%03iROT',  'D%03iXSH',
                    'D%03iYSH',  'D%03iSFTU', 'D%03iSFTF', 'D%03iEXKY',
                    'D%03iINUN', 'D%03iOUUN', 'D%03iFVAL', 'D%03iINXC',
                    'D%03iINYC', 'D%03iOUXC', 'D%03iOUYC', 'D%03iGEOM',
                    'D%03iVER']

        # go over an index indicating
        # an input image for multidrizzle
        for index in range(self.ndrizzle-1):

            # go over all keyword frames
            for key in keynames:

                # form the actual keyword
                drizz_keyname = key % (index+1)

                # check whether the keywird exists
                try:
                    if drizz_keyname in head:
                        # delete the keyword
                        del head[drizz_keyname]
                except:
                    print("failed to delete key %s"%(drizz_keyname))
                    #continue and pray
        # update the image
        img.flush()

        # close the image
        img.close()
        print(' Done')


class Flux_Cube(object):
    """
    The class for the fluxcube images. The creation of those
    fluxcube images is the main purpose of the module.
    Correspondingly the class is oriented to mainly serve
    this purpose.
    """
    def __init__(self, grism_image, index, useMdriz):
        """
        Initializes the class

        The method extracts the name of an input file from the
        header of a multidrizzled grism image. Then the name
        of the corresponding fluxcube is derived and stored.
        Also all other drizzle information for the input image
        image is extraced and stored

        @param grism_image: the name of the multidrizzled grism image
        @type grism_image: string
        @param index: the index number in the header of the mult. gr. im.
        @type index: int

        the index is the input data file in the header of the grism image,
        a flux cube will eventually be made for each input image that went into
        the drizzled grism image which was specified.

        """
        import os
        import os.path

        from astropy.io import fits as pyfits

        #use mdriz or adriz
        self.useMdriz = useMdriz
        self.grism_image_name = grism_image
        # open the image and get the header
        grism_img  = pyfits.open(grism_image, 'readonly')
        grism_head = grism_img[0].header

        if self.useMdriz:
            # subsequently extract all information
            # on the particular input
            keyname   = 'D%03iDATA' % (index)
            self.data = grism_head[keyname]

            keyname   = 'D%03iDEXP' % (index)
            self.dexp = grism_head[keyname]

            keyname   = 'D%03iOUDA' % (index)
            self.ouda = grism_head[keyname]

            keyname   = 'D%03iOUWE' % (index)
            self.ouwe = grism_head[keyname]

            keyname   = 'D%03iOUCO' % (index)
            self.ouco = grism_head[keyname]

            keyname   = 'D%03iMASK' % (index)
            self.mask = grism_head[keyname]

            keyname   = 'D%03iWTSC' % (index)
            self.wtsc = grism_head[keyname]

            keyname   = 'D%03iKERN' % (index)
            self.kern = grism_head[keyname]

            keyname   = 'D%03iPIXF' % (index)
            self.pixf = grism_head[keyname]

            keyname   = 'D%03iCOEF' % (index)
            self.coef = grism_head[keyname]

            keyname   = 'D%03iXGIM' % (index)
            self.xgim = grism_head[keyname]

            keyname   = 'D%03iYGIM' % (index)
            self.ygim = grism_head[keyname]

            keyname   = 'D%03iLAM' % (index)
            self.lam  = grism_head[keyname]

            keyname   = 'D%03iSCAL' % (index)
            self.scal = grism_head[keyname]

            keyname   = 'D%03iROT' % (index)
            self.rot  = grism_head[keyname]

            keyname   = 'D%03iXSH' % (index)
            self.xsh  = grism_head[keyname]

            keyname   = 'D%03iYSH' % (index)
            self.ysh  = grism_head[keyname]

            keyname   = 'D%03iSFTU' % (index)
            self.sftu = grism_head[keyname]

            keyname   = 'D%03iSFTF' % (index)
            self.sftf = grism_head[keyname]

            keyname   = 'D%03iEXKY' % (index)
            self.exky = grism_head[keyname]

            keyname   = 'D%03iINUN' % (index)
            self.inun = grism_head[keyname]

            keyname   = 'D%03iOUUN' % (index)
            self.ouun = grism_head[keyname]

            keyname   = 'D%03iFVAL' % (index)
            self.fval = grism_head[keyname]

            keyname   = 'D%03iINXC' % (index)
            self.inxc = grism_head[keyname]

            keyname   = 'D%03iINYC' % (index)
            self.inyc = grism_head[keyname]

            keyname   = 'D%03iOUXC' % (index)
            self.ouxc = grism_head[keyname]

            keyname   = 'D%03iOUYC' % (index)
            self.ouyc = grism_head[keyname]
        else:
            # subsequently extract all information
            # on the particular input
            keyname   = 'D%03iDATA' % (index)
            self.data = grism_head[keyname]

            keyname   = 'D%03iDEXP' % (index)
            self.dexp = grism_head[keyname]

            keyname   = 'D%03iOUDA' % (index)
            self.ouda = grism_head[keyname]

            keyname   = 'D%03iOUWE' % (index)
            self.ouwe = grism_head[keyname]

            keyname   = 'D%03iOUCO' % (index)
            self.ouco = grism_head[keyname]

            keyname   = 'D%03iMASK' % (index)
            self.mask = grism_head[keyname]

            keyname   = 'D%03iWTSC' % (index)
            self.wtsc = grism_head[keyname]

            keyname   = 'D%03iKERN' % (index)
            self.kern = grism_head[keyname]

            keyname   = 'D%03iPIXF' % (index)
            self.pixf = grism_head[keyname]

            keyname   = 'D%03iCOEF' % (index)
            self.coef = grism_head[keyname]

            keyname   = 'D%03iSCAL' % (index)
            self.scal = grism_head[keyname]


            keyname   = 'D%03iFVAL' % (index)
            self.fval = grism_head[keyname]


        # close the image
        grism_img.close()

        # get information on the flux cube
        self.fcube_info = self._get_fcube_info(self.data)

        # get the fluxcube name
        #self.fcube_name = self._get_fcubename_old(self.data)
        self.fcube_name = self._get_fcubename(self.fcube_info)

        # get the name of the grism input image
        #self.inima_name = string.split(self.data, '.fits')[0] + '.fits'
        if not os.path.isfile(self.fcube_info['fits']):
            err_msg = 'FCUBEPREP: File ' + self.fcube_info['fits'] + ' does not exist!'
            raise aXeError(err_msg)

        # get the name of the grism input image
        # self.inima_extnum = self._get_extnum(self.data)

        #self.coeff_name  = self._get_coeff_name(self.inima_name, self.inima_extnum)
        if self.useMdriz:
            self.coeff_name  = self._get_coeff_name(self.fcube_info)
            if not os.path.isfile(self.coeff_name):
                err_msg = 'FCUBEPREP: File ' + self.coeff_name + ' does not exist!'
                raise aXeError(err_msg)

        #self.inima_dims = self._get_indims(self.inima_name, self.inima_extnum)
        self.inima_dims = self._get_indims(self.fcube_info)

    def _get_fcube_info(self, header_name):
        """
        Get all info on the extension

        @param header_name: the header name
        @type header_name: string

        @return: information on the fluxcube
        @rtype: dict
        """
        # create an empty dict
        fcube_info = {}

        # get the bracket information
        exte_str  = header_name.split('.fits')[1]

        # get the inside of the brackets
        exte_data = exte_str[1:len(exte_str)-1]

        # collect fits name, extension name and extension
        # version in the dictionary
        fcube_info['root'] = header_name.split('.fits')[0]
        fcube_info['fits'] = header_name.split('.fits')[0] + '.fits'
        fcube_info['ext_nam'] = exte_data.split(',')[0]
        fcube_info['ext_ver'] = int(exte_data.split(',')[1])

        # return the extension information
        return fcube_info

    def _renormalize_image(self, in_image):
        """
        Re-normalize the blotted image - assumes SIMPLE FITS

        Blotting applies a scaling witha scaling factor
        stored in the header of the blotted image.
        When blotting the segmentation images, this
        scaling falsifies the object ID's stored as
        pixel values. This function undoes the
        renormalization to get back the integer
        object ID's

        @param in_image: the name of the multidrizzled grism image
        @type in_image: string
        """

        from astropy.io import fits as pyfits
        from . import axeutils

        # open the image and take the header
        inima = pyfits.open(in_image, mode='readonly')
        in_header = inima[0].header

        # check for the keyword
        if self.useMdriz:
            if 'BLOTSCAL' in in_header:
                # read the keyword and comute the
                # correction factor
                blotscal = in_header['BLOTSCAL']
                factor   = blotscal*blotscal
            else:
                # raise an exception
                # if the keyword is missed
                err_msg = 'FCUBEPREP: Blotted image needs keyword "BLOTSCAL"!'
                raise aXeError(err_msg)
        else:
            err_msg = "Astrodrizzle doesn't have a blotting scale factor"
            raise aXeError(err_msg)

        # close the fits image
        inima.close()

        # apply the correction
        inima = pyfits.open(in_image, mode='update')
        inima[0].data *= factor
        inima.close()


    def _blot_image(self, in_image, out_image, x_excess, y_excess, interpol):
        """
        Blot one image

        Thats just a simple wrapper around the task blot
        in the dither package. Most of the parameter values
        are part of the instance data, the ones differing
        which each call come via input, and the other
        are fixed and set to the 'multidrizzle' values.

        @param in_image: the input image name
        @type in_image: string
        @param out_image: the name of the blotted image
        @type out_image: string
        @param x_excess: the pixel excess in x
        @type x_excess: int
        @param y_excess: the pixel excess in y
        @type y_excess: int
        @param interpol: the interpolation method to use
        @type interpol: string
        """
        from pyraf import iraf
        from iraf import stsdas,analysis,dither

        #print self.x_offs
        coeffs_lines = open(self.coef).readlines()
        for i, line in enumerate(coeffs_lines):
            if line.strip().startswith('refpix'):
                ##print line
                old_x = float(coeffs_lines[i].split()[1])
                old_y = float(coeffs_lines[i].split()[2])
                new_x = old_x - self.x_offs
                new_y = old_y - self.y_offs
                coeffs_lines[i] = 'refpix %9.3f %9.3f\n'%(new_x,new_y)
        new_coef = self.coef+".FLX"
        fp = open(new_coef,"w")
        fp.writelines(coeffs_lines)
        fp.close()


        irafout=''
        #we need to take into account MEF and Simple segment maps wince SEXTRACTOR now
        #supports both types
        ftype=fileutil.isFits(in_image)[1]
        inheader=pyfits.getheader(in_image)

        if ( ftype == 'mef'):
            inheader=pyfits.getheader(in_image,extname=str(self.fcube_info["ext_nam"]),extver=self.fcube_info["ext_ver"])
            in_image = in_image + "[" + str(self.fcube_info["ext_nam"])+ "," + str(self.fcube_info["ext_ver"]) + "]"

        if 'EXPTIME' not in inheader:
            print("\nInput image: %s doesn't have EXPTIME in header, blot needs this to run\n"%(in_image))
            return 1

        #print in_image, out_image, self.scal, self.coef
        #print  self.xsh, self.ysh, self.rot,
        #print self.inima_dims[0]+2*x_excess, self.inima_dims[1]+2*y_excess
        #print "using in_image as ", in_image
        try:
            irafout=iraf.blot(data=in_image, outdata=out_image,
                          scale=self.scal,
                          coeffs=new_coef, xsh=self.xsh, ysh=self.ysh,
                          rot=self.rot,outnx=self.inima_dims[0]+2*x_excess,
                          outny=self.inima_dims[1]+2*y_excess,
                          align='center',
                          shft_un='input', shft_fr='input', in_un='cps',
                          out_un='cps', interpol=interpol, sinscl='1.0',
                          expout=self.dexp, expkey='EXPTIME', fillval=0.0,
                          Stdout=1)

        except:
            print('When blotting image %s --> %s something went wrong!' % (in_image, out_image))
            print(irafout)
            # indicate something went wrong
            return 1
        # the default return if
        # things went all right
        return 0


    def _a_blot_image(self, image_to_blot, tempname, x_excess, y_excess, interp):
        """
        Blot one image.

        Thats just a simple wrapper around the task blot in astrodrizzle

        @param image_to_blot: the input image name, either the grism or direct drizzled image
        @type in_image: string

        @param tempname: the name of the output blotted image
        @type out_image: string


        """
        from drizzlepac import astrodrizzle

        #the drizzle coeff information for adriz is taken from the self.data image,
        excess_x=0
        excess_y=0

        ##use the current data as reference image for output
        #self.data comes from the header of the input grism or flux image
        #and is one of the input images used to make the drizzled image_to_blot
        input_image=(self.data).split("[")[0]
        exptime=pyfits.getval(input_image,'EXPTIME')
        bunit=pyfits.getval(image_to_blot,'BUNIT')

        flt_header=pyfits.getheader(input_image)
        flt_wcs=HSTWCS(self.data)


        #now look at the image to blot, this is a drizzled image
        ftype=fileutil.isFits(image_to_blot)[1]

        if ( ftype == 'mef'):
            blot_wcs=HSTWCS(image_to_blot,ext=(str(self.fcube_info["ext_nam"]),str(self.fcube_info["ext_ver"])))
            image_data=pyfits.getdata(image_to_blot,extname=str(self.fcube_info["ext_nam"]),extver=int(self.fcube_info["ext_nam"]))

        elif (ftype == 'simple'):
            blot_wcs=HSTWCS(image_to_blot) #assume simple
            image_data = pyfits.getdata(image_to_blot)

        else:
            return IOError("File type of fits image is not supported %s"%(image_to_blot))


        #edit the wcs header information to add any dim_info shifts that we need, expanding the size of the output image
        #make sure this gets saved to the output extension header. The lambda image will be bigger than the segment image
        if x_excess > 0:
            excess_x = flt_wcs.naxis1+x_excess*2
            flt_wcs.naxis1=excess_x
            crpix=flt_wcs.wcs.crpix
            newx=crpix[0] + x_excess
            flt_wcs.wcs.crpix=np.array([newx,crpix[1]])
            flt_wcs.sip.crpix[0]=newx

        if y_excess > 0:
            excess_y = flt_wcs.naxis2+y_excess*2
            flt_wcs.naxis2=excess_y
            crpix=flt_wcs.wcs.crpix
            newy=crpix[1] + y_excess
            flt_wcs.wcs.crpix=np.array([crpix[0],newy])
            flt_wcs.sip.crpix[1]=newy

        #outimage is just the data array
        outimage=astrodrizzle.ablot.do_blot(image_data.astype(np.float32), blot_wcs, flt_wcs, 1.,
                interp=interp, sinscl=1.,coeffs=True,wcsmap=None,stepsize=10)


        #update the flt_header with the flt_wcs information I created
        flt_header['CRPIX1']=flt_wcs.wcs.crpix[0]
        flt_header['CRPIX2']=flt_wcs.wcs.crpix[1]

        try:
            newimage=pyfits.PrimaryHDU()
            newimage.data=outimage
            newimage.header=flt_header
            newimage.header['BUNIT']=bunit
            newimage.header.update(flt_wcs.to_header())
            newimage.verify('silentfix')
            newimage.writeto(tempname)
        except:
            raise IOError("Problem writing fits image %s"%(tempname))



    def _a_blot_segment_image(self, image_to_blot, tempname, x_excess, y_excess, interp):

        """
        Blot the segmentation or other nondrizzled image as if it were

        assume self.grism_image is always used as the source wcs reference

        Thats just a simple wrapper around the task blot in astrodrizzle

        @param image_to_blot: the input image name, either the grism or direct drizzled image
        @type in_image: string

        @param tempname: the name of the output blotted image
        @type out_image: string

        exposure time is hard coded to 1 since it was made from the drizzled image and we dont
        want the id numbers rescaled by the exposure time that was used for blotting

        """
        from drizzlepac import astrodrizzle

        excess_x=0
        excess_y=0

        #the drizzle coeff information for adriz is taken from the self.data image,

        input_image=(self.data).split("[")[0] #use the current data as reference image
        exptime=1. #dont scale the segment image pyfits.getval(input_image,'EXPTIME')

        flt_header=pyfits.getheader(input_image)
        flt_wcs=HSTWCS(self.data)

        #check to see if this is a simple fits or MEF and grab the science information
        #The output image
        ftype=fileutil.isFits(self.grism_image_name)[1]

        if (ftype == 'mef'):
            grism_wcs=HSTWCS(self.grism_image_name,ext=(str(self.fcube_info["ext_nam"]),self.fcube_info["ext_ver"]))
        elif (ftype == 'simple'):
            grism_wcs=HSTWCS(self.grism_image_name) #assume simple and info in primary
        else:
            return IOError("File type of fits image is not supported %s"%(image_to_blot))

        ftype=fileutil.isFits(image_to_blot)[1]
        if (ftype == 'mef'):
            image_data=pyfits.getdata(image_to_blot,ext=(str(self.fcube_info["ext_nam"]),self.fcube_info["ext_ver"])) #get the first science image
        elif (ftype == 'simple'):
            image_data = pyfits.getdata(image_to_blot)
        else:
            return IOError("Input image is not a supported FITS type: %s"%(image_to_blot))


        #edit the wcs header information to add any dim_info shifts that we need,
        #the segment image needs to be the same sky area cut without the added pixels
        if x_excess > 0:
            excess_x = flt_wcs.naxis1+x_excess*2
            flt_wcs.naxis1=excess_x
            crpix=flt_wcs.wcs.crpix
            newx=crpix[0] + x_excess
            flt_wcs.wcs.crpix=np.array([newx,crpix[1]])
            flt_wcs.sip.crpix[0]=newx

        if y_excess > 0:
            excess_y = flt_wcs.naxis2+y_excess*2
            flt_wcs.naxis2=excess_y
            crpix=flt_wcs.wcs.crpix
            newy=crpix[1] + y_excess
            flt_wcs.wcs.crpix=np.array([crpix[0],newy])
            flt_wcs.sip.crpix[1]=newy


        #returns a numpy.ndarray which is just the data
        outimage=astrodrizzle.ablot.do_blot(image_data.astype(np.float32), grism_wcs, flt_wcs, 1.,
                interp=interp, sinscl=1.,coeffs=True,wcsmap=None,stepsize=10)

        #update the flt_header with the flt_wcs information I created
        flt_header['CRPIX1']=flt_wcs.wcs.crpix[0]
        flt_header['CRPIX2']=flt_wcs.wcs.crpix[1]

        #if the input flt was an MEF we need to write an MEF out
        try:
            newimage=pyfits.PrimaryHDU()
            newimage.data=outimage
            newimage.header=flt_header
            newimage.header.update(flt_wcs.to_header())
            newimage.verify('silentfix')
            newimage.writeto(tempname)
        except:
            raise IOError("Problem writing fits image %s"%(tempname))



    def _get_fcubename(self, fcube_info):
        """
        Get the name of the fluxcube

        @param fcube_info: the fluxcube info
        @type fcube_info: dict

        @return: the name of the fluxcube
        @rtype: string
        """
        fcube_name = '%s_%i.FLX.fits' % (fcube_info['root'], 3*fcube_info['ext_ver']-1)
        return fcube_name

    def _get_indims(self, fcube_info):
        """
        Get the dimensions of the image

        @param fcube_info: the fluxcube info
        @type fcube_info: dict

        @return: the image dimension
        @rtype: [int,int]
        """
        import os
        import os.path

        from astropy.io import fits as pyfits

        # make sure the fits image exists
        if not os.path.isfile(fcube_info['fits']):
            err_msg = 'Image: ' + fcube_info['fits'] + ' does not exist!'
            raise aXeError(err_msg)

        # open the image and get the header
        in_img  = pyfits.open(fcube_info['fits'], 'readonly')
        in_head = in_img[fcube_info['ext_nam'],fcube_info['ext_ver']].header

        # extract the keywords for the image size from
        # the header
        dims = [in_head['NAXIS1'],in_head['NAXIS2']]

        # close the image
        in_img.close()

        # return the list with the image dimension
        return dims

    def _get_indims_old(self, image_name, extnum):
        """
        Input:
            image_name - the image name
            extnum     - the extension number

        Return:
            dims       - a two-entry list with the
                         x/y dimensions of the imag[sci,extnum]

        Description:
            The method determines the size of the science
            extension of an image.
        @param image: the name of the multidrizzled grism image
        @type image: string

        @return: the number of input images
        @rtype: int
        """
        from astropy.io import fits as pyfits

        if not os.path.isfile(image_name):
            err_msg = 'Image: ' + grism_image + ' does not exist!'
            raise aXeError(err_msg)

        # open the image and get the header
        in_img  = pyfits.open(image_name, 'readonly')
        in_head = in_img['SCI',extnum].header

        # extract the keywords for the image size from
        # the header
        dims = [in_head['NAXIS1'],in_head['NAXIS2']]

        # close the image
        in_img.close()

        # return the list with the image dimension
        return dims
    """
    def _get_coeff_name(self, in_image, extnum):
        from astropy.io import fits as pyfits

        # open the image and get the header
        in_img  = pyfits.open(in_image, 'readonly')
        in_head = in_img['SCI',extnum].header
        if 'CCDCHIP' in in_head:
            chipnum = in_head['CCDCHIP']
        else:
            chipnum = 1
            #   j8mt44k0q_flt_coeffs1.dat
        coeffname = in_image.split('.fits')[0] + '_coeffs' + str(chipnum) + '.dat'

        return coeffname
    """
    def _get_coeff_name(self, fcube_info):
        """
        Compose the coefficient filename

        @param fcube_info: the fluxcube info
        @type fcube_info: dict

        @return: the name of the coefficients file
        @rtype: string
        """
        from astropy.io import fits as pyfits

        # set default number
        chipnum = 1

        # open the image and get the header
        in_img  = pyfits.open(fcube_info['fits'], 'readonly')

        # ACS:
        # check for the chip number
        if  'CCDCHIP' in in_img[fcube_info['ext_nam'],fcube_info['ext_ver']].header:
            chipnum = in_img[fcube_info['ext_nam'],fcube_info['ext_ver']].header['CCDCHIP']

        # NICMOS:
        # check for camera number
        elif 'CAMERA' in in_img[0].header:
            chipnum = in_img[0].header['CAMERA']

        # close the fits
        in_img.close()

        # compose the coefficient name
        coeffname = '%s_coeffs%i.dat' % (fcube_info['root'], chipnum)

        # return the number
        return coeffname

    def create_fitscube(self, segm_image, filter_images, dim_info, interpol):
        """
        Creates one fitscube

        This method creates a fluxcube fits-image. The input is evaluated
        and then the various calls to the blot routine - one for the
        segmentation image and one for every flux image - are executed.
        Also the headers are filled according to the specifications.

        @param segm_image: the name of the segmentation image
        @type segm_image: string
        @param filter_images: list of the triples fluximage,wavelength,ST_zero
        @type filter_images: []
        @param dim_info: the list with the excess pixels
        @type dim_info: []
        @param interpol: the interpolation method for the flux images
        @type interpol: string
        """
        import os
        import os.path

        from astropy.io import fits as pyfits
        from . import axeutils
        # look for the maximum excess in x and y
        x_excess = max([dim_info[0], dim_info[1]])
        y_excess = max([dim_info[2], dim_info[3]])


        # given that in the blot you add the maximum
        # requested pixels on each side, derive the
        # image section area string to get back to the requested
        # pixels on either side
        x_start = x_excess - dim_info[0] + 1
        y_start = y_excess - dim_info[2] + 1

        x_offs = -1*dim_info[0]
        y_offs = -1*dim_info[2]

        #this is a fix for the dimension_ offset in mdriz only
        self.x_offs = x_offs
        self.y_offs = y_offs

        x_end = self.inima_dims[0] + 2*x_excess - (x_excess - dim_info[1])
        y_end = self.inima_dims[1] + 2*y_excess - (y_excess - dim_info[3])

        print('Creating ', self.fcube_name, ' ...', end= ' ')

        # delete a just existing, previous fluxcube image
        if os.path.isfile(self.fcube_name):
            os.unlink(self.fcube_name)

        # set up the fluxcube image, and store the primary
        # header; also store the keywords XOFFS/YOFFS in
        # the primary header
        mex_hdu = pyfits.HDUList()
        hdrpr = pyfits.PrimaryHDU()
        mex_hdu.append(hdrpr)
        hdr = mex_hdu[0].header
        hdr['XOFFS'] = (x_offs, 'X-OFFSET between flt and fluxcube')
        hdr['YOFFS'] = (y_offs, 'Y-OFFSET between flt and fluxcube')
        mex_hdu.writeto(self.fcube_name)

        # get a tmp-filename
        tmpname =  axeutils.get_random_filename('','.fits')
        # blot the segmenation image
        if self.useMdriz:
            if (self._blot_image(segm_image, tmpname, x_excess, y_excess, 'nearest')):
                print('problem blotting',segm_image)

            else:
                self._renormalize_image(tmpname)

        else: #use astrodrizzle blot,
            self._a_blot_segment_image(segm_image,tmpname,x_excess, y_excess, 'nearest')


        # copy the appropriate image section to the fluxcube
        tmp_fits = pyfits.open(tmpname, 'readonly')
        tmp_fits[0].header['EXTNAME']='SEGM'
        tmp_fits[0].header['EXTVER']=1
        pyfits.append (self.fcube_name,
                       tmp_fits[0].data[y_start-1:y_end,x_start-1:x_end],
                       tmp_fits[0].header)
        tmp_fits.close()

        # delete the tmp-file
        os.unlink(tmpname)

        # go over all filter images
        for  fimage in filter_images:

            # get the name of the fluximage and the
            # wavelength; prepare the keword entry for
            # the wavelength
            fluximg    = fimage.get_fluxname()
            wavelength = fimage.get_wavelength()
            ext_name   = '[LAMBDA'+ str(int(wavelength))+',1,append]'

            # get a tmp-filename
            tmpname =  axeutils.get_random_filename('','.fits')

            # blot the fluximage
            if self.useMdriz:
                res=self._blot_image(fluximg, tmpname, x_excess, y_excess, interpol)
                self._renormalize_image(tmpname)

                # just return in case there was an error
                if res:
                    print('Interrupted multidrizzle blot')
                    return res

            else: #astro blot all the input flux filter images
                print("Using excess pixels of x,y ", x_excess, y_excess)
                self._a_blot_image(fluximg,tmpname,x_excess, y_excess,interpol) #use a reference image for blotting



            # store the wavelength in the header
            tmp_fits = pyfits.open(tmpname, 'update')
            tmp_fits[0].header['WAVELENG'] = (wavelength, 'wavelength the image was taken')
            tmp_fits.close()

            # copy the appropriate section to te fluxcube
            tmp_fits = pyfits.open(tmpname, 'readonly')
            tmp_fits[0].header['EXTNAME']='LAMBDA'+str(int(wavelength))
            tmp_fits[0].header['EXTVER']=1
            pyfits.append (self.fcube_name, tmp_fits[0].data,
                           tmp_fits[0].header)
            tmp_fits.close()

            # delete the tmp-file again
            os.unlink(tmpname)

        print(' Done')


class Flux_Image(object):
    """
    The class for the flux images. The flux images are the
    multidrizzled filter images, however transformed
    from counts per second to flux. An according segment of each
    flux image is stored in the fluxcubes.
    """
    def __init__(self, image_name, wavelength, mag_zero, AB_input, useMdriz):
        """
        Input:
            image_name - the name of the direct image
            wavelength - the wavelength of the direct image
            mag_zero   - the zero point for the filter
            AB_input   - 1 if the zeropoint is in AB, 0 if in ST

        Return:

        Description:
            The input data is stored as class data.
        """
        self.image_name = image_name + '[SCI]'
        self.wavelength = wavelength
        if AB_input:
            self.st_zero = self._get_stmag_from_magab(mag_zero, wavelength)
        else:
            self.st_zero    = mag_zero

        self.flux_name = self._get_fluxname(self.image_name)
        self.useMdriz=useMdriz

    def _get_fluxname(self, image_name):
        """
        Get the name of the flux image

        The method derives the name of the flux image which
        is derived for each direct image.
        Example: gaga_drz.fits[SCI]  -->  gaga_drz.FLX.fits
        """
        return image_name.split('.fits')[0] + '.FLX.fits'

    def _get_flambda_from_magab(self, mag, wlength):
        """
        Compute f_lambda from mag_AB and lambda [nm]

        /**
        *
        * Function: get_flambda_from_magab
        * The subroutine calculates the flambda value for a
        * mag_AB value given with its wvavelength as input
        * parameters.
        *
        * Parameters:
        * @param  mag     - the mag_AB value
        * @param  lambda  - the wavelength for mag_AB
        *
        * Returns:
        * @return flambda - the calculated flambda value
        */
        double
        get_flambda_from_magab(double mag, double lambda)
        {
        double flambda=0.0;
        double fnu=0.0;

        fnu     = pow(10.0, -0.4*(mag+48.6));
        flambda = 1.0e+16*LIGHTVEL*fnu/(lambda*lambda);

        return flambda;
        };
        """
        import math

        fnu     = math.pow(10.0, -0.4*(mag+48.6))
        if wlength != 0:
           flambda = 2.99792458e+16*fnu/(wlength*wlength)
        else:
           flambda = 0.

        return flambda

    def _get_stmag_from_magab(self, mag, wlength):
        """
        Compute STmagzero from ABmagzero
        """
        import math

        if wlength != 0:
           flambda = self._get_flambda_from_magab(mag, wlength)
           stmag = -2.5*math.log10(flambda) - 21.10
        else:
           stmag = 0.

        return stmag

    def get_fluxname(self):
        """
        Get the flux image name

        The method delivers the name of
        the fluximage to the outside world.
        """
        return self.flux_name

    def get_wavelength(self):
        """
        Get the proper wavelength

        The method delivers the wavelength of
        the fluximage to the outside world.
        """
        return self.wavelength

    def transform_toflux(self, segm_image):
        """
        Transform an image from cps to flux

        The method creates a flux image for a direct multidrizzled
        image. The direct image comes directly from multidrizzle,
        its unit is [cts/s], however the fluxcubes need the information
        in ergs/cm^2/s/AA.

        @param segm_image: the segmentation for the data set
        @type segm_image: string
        """
        import os
        import os.path

        from astropy.io import fits as pyfits

        # get the name of the flux image
        self.flux_name = self._get_fluxname(self.image_name)

        # delete the flux image if it already exists
        if os.path.isfile(self.flux_name):
            os.unlink(self.flux_name)

        # the expression to apply to the image data
        expon = 10.0 ** (-0.4 * (21.10 + self.st_zero))

        print(' image_name: %s' % (self.image_name.split('[')[0]))
        print(' segm_image: %s' % segm_image)
        print(' flux_name: %s' % self.flux_name)


        # open the input files and apply the conversion
        file_a = pyfits.open(self.image_name.split('[')[0], 'readonly')
        file_b = pyfits.open(segm_image, 'readonly')
        ftype=fileutil.isFits(file_b.filename())[1]
        if (ftype == 'mef'):
            bgt1 = file_b[1].data >= 1 #assume in first
            blt1 = file_b[1].data < 1 #assume in first
        else:
            bgt1 = file_b[0].data >= 1
            blt1 = file_b[0].data < 1

        ftype=fileutil.isFits(file_a.filename())[1]

        if (ftype == 'mef'):
            file_a['SCI'].data[bgt1] *= expon
            file_a['SCI'].data[blt1] = 0.0
        else:
            file_a[0].data[bgt1] *= expon
            file_a[0].data[blt1] = 0.0

        # concatenate the primary and extension headers for output
        # to a simple FITS file
        prihdr = file_a[0].header

        del prihdr['nextend']

        if (ftype == 'mef'):
            scihdr = file_a['SCI'].header
            scihdr._strip()
#            newhdr = pyfits.Header(prihdr.ascard + scihdr.ascard)
            tmp = prihdr.copy()
            tmp.extend(scihdr.cards)
            newhdr = pyfits.Header(tmp)

            data=file_a['SCI'].data
        else:
#            newhdr = pyfits.Header(prihdr.ascard)
            newhdr = pyfits.Header(prihdr.cards)
            data=file_a[0].data

        #scihdr._strip() #remove cards which deal with header type like SIMPLE, BITPIX
        #for key in ('EXTNAME','EXTVER','INHERIT','DATE','ORIGIN','IRAF-TLM'):
        #    del scihdr[key]


        # write the new flux image in simple fits format regardless of input format
        pyfits.writeto(self.flux_name, data, newhdr)
        file_a.close()
        file_b.close()

        print("")
        print(self.image_name, ' --> ', self.flux_name)
        print("")
        # Formulas:
        # STmag = -2.5*log10(image) + self.st_zero
        # STmag = -2.5*log10(F_lam) -21.10
        #
        # ==> F_lam = 10**(-0.4*(st_zero+21.10)) * image


class FluxCube_Maker(object):
    """
    Central class to take the input and to create the fluxcubes
    for the list of images extracted from the header of the
    multidrizzled grism image.
    """
    def __init__(self, grism_image, segm_image, filter_info, AB_input,
                 dim_term, interpol, useMdriz):
        """
        Initializes the class

        The class data is set. While doing that, basic checks
        on the input is done. The existence of the images is
        checked, also the data type of the various real
        or integer numbers.

        @param grism_image: the name of the multidrizzled grism image
        @type grism_image: string
        @param segm_image: the name of the segmentation image
        @type segm_image: string
        @param filter_info: information on multidrizzled direct images
        @type filter_info: []/string
        @param dim_term: description of the additional rows/column for the fluxcubes
        @type dim_term: string
        @param interpol: the interpolation method for the flux images
        @type interpol: string
        """
        import os
        import os.path

        self.useMdriz = useMdriz #bool whether to blot or ablot, that is the question
        self.filter_images = []
        self.dim_info      = []
        self.fcube_list    = []

        # check whether the grism image exist,
        # store the name if it exists
        if not os.path.isfile(grism_image):
            err_msg = 'File: ' + grism_image + ' does not exist!'
            raise aXeError(err_msg)
        else:
            self.grism_image = grism_image

        # check whether the segmentation image exist,
        # store the name if it exists
        if not os.path.isfile(segm_image):
            err_msg = 'File: ' + segm_image + ' does not exist!'
            raise aXeError(err_msg)
        else:
            self.segm_image = segm_image

        # check whether the input for the flux images is a file
        if not os.path.isfile(filter_info):
            # split the input into its items
            finfo = filter_info.split(',')

            if len(finfo) != 3:
                err_msg = 'There must be 3 items in '+ filter_info +', not '+ str(len(finfo)) +'!'
                raise aXeError(err_msg)

            # in case of a string, create a fluximage
            #self.filter_images.append(self._make_fluxim(filter_info, AB_input))
            self.filter_images.append(self._make_fluxim(finfo[0], finfo[1], finfo[2], AB_input))
        else:
            # in case of a file, let the information be extraced
            # and create the fluximages
            self.filter_images.extend(self._evaluate_finfolist(filter_info, AB_input))

        # resolve and get the dimension information
        self.dim_info = self._get_dimension_info(dim_term)

        # store the interpolation method
        self.interpol = interpol

    def _get_dimension_info(self,dimension_term):
        """
        Get the dimension information
        """
        # initialize the array
        dim_info = []

        # check the dimension input
        dim_entries = dimension_term.split(',')

        # is the number of items correct?
        if len(dim_entries) != 4:
            err_msg = 'There must be 4 entries in the term: %s, not %i!' % (dimension_term, len(dim_entries))
            raise aXeError(err_msg)

        # check whether each item is an integer
        for item in dim_entries:
            if self._toInt(item.strip()) == None:
                err_msg = 'Item: ' + item + ' must be integer!'
                raise aXeError(err_msg)
            # store the item
            dim_info.append(self._toInt(item.strip()))

        # return the array
        return dim_info

    def _trim_imheaders(self, segm_image, filter_images):
        """
        Delete unnecessary header keywords

        The method trims the keyword headers of all
        input images down such that there won't be
        problems with blot later on.
        """

        # create an object
        one_image = MDrizzle_Image(segm_image)
        # trim the header
        #one_image.delete_drizzlekeys()

        # go over each filter image object
        for fimage in filter_images:
            # create an object
            one_image = MDrizzle_Image(fimage.flux_name)
            # trim the header
            #one_image.delete_drizzlekeys()

    def _fill_fcubelist(self, grism_image):
        """
        Makes a list of fluxcubes to be created

        The method determines the number of input images
        in the header of the multidrizzled grism images
        and then creates for each input image a fluxcube
        object
        """
        fcubes = []

        # determine the number of iput images
        n_fcubes = self._get_nfcube(grism_image)

        # create a fluxcube object for each input image
        for index in range(1,n_fcubes+1):
            fcubes.append(Flux_Cube(grism_image, index, self.useMdriz))

        # return the fluxcube list
        return fcubes

    def _get_nfcube(self, grism_image):
        """
        Get the number of fluxcubes to be created

        The method looks in the header of the grism image
        for the name of the input images used in the multidrizzle
        process. This number is returned
        """
        from astropy.io import fits as pyfits

        # open the fits file and get the header
        grism_img  = pyfits.open(grism_image, 'readonly')
        grism_head = grism_img[0].header

        # create the keyname for the first input image
        ID = 1
        keyname = 'D%03iDATA' % (ID)

        # create the keyname for subsequent input images
        # and continue until the keynames do not exist
        while keyname in grism_head:
            ID = ID+1
            keyname = 'D%03iDATA' % (ID)

        # close the grism image
        grism_img.close()

        # correct the number
        ID = ID-1

        # return the number
        return ID


    def _toFloat(self, input):
        """
        Check for float

        The module checks whether an expression is a
        float or not. The float representation of the
        expresion or None is returned
        """
        try: fret = float(input)
        except: return None
        else:   return fret


    def _toInt(self, input):
        """
        Check for integer

        The module checks whether an expression is an
        integer or not. The integer representation of the
        expression or None is returned
        """
        try: iret = int(input)
        except: return None
        else:   return iret


    def _make_fluxim(self, img_name, wav_pivot, zeropoint, AB_zero):
        import os
        import os.path
        """
        Generate a flux image
        """
        # check whether the first item is the name of an existing file
        if not os.path.isfile(img_name):
            err_msg = 'File: ' + img_name + ' does not exist!'
            raise aXeError(err_msg)

        # check whether the second item is a float
        if self._toFloat(wav_pivot) == None:
            err_msg = 'Expression: ' + wav_pivot + ' must be float!'
            raise aXeError(err_msg)

        # check whether the third item is a float
        if self._toFloat(zeropoint) == None:
            err_msg = 'Expression: ' + zeropoint + ' must be float!'
            raise aXeError(err_msg)

        # create a new fluximage and return it
        return Flux_Image(img_name, float(wav_pivot), float(zeropoint), AB_zero, self.useMdriz)


    def _evaluate_finfolist(self, filter_file, AB_zero):
        """
        Evaluate the filter list

        The method opens and extracts direct image information
        from the file given in the input. For each direct image
        specified there a flux image object is created, and
        the list of flux image objects is returned.

        @param filter_file: the name of the file with the direct image information
        @type filter_file: string
        @param AB_zero: the zeropoint in AB
        @type AB_zero: float

        @return: the list of filter image objects created
        @rtype: []
        """
        from axe import axe_asciidata

        # make an empty list
        filter_images = []

        # open the file
        f_list = axe_asciidata.open(filter_file, delimiter=',')

        # go over all rows
        for index in range(f_list.nrows):

            # get image name, pivot wavelength and zeropoint
            img_name  = f_list[0][index].strip()
            wav_pivot = f_list[1][index]
            zeropoint = f_list[2][index]

            filter_images.append(self._make_fluxim(img_name, wav_pivot, zeropoint, AB_zero))

        # return the object list
        return filter_images

    def run(self):
        """
        Make all fluxcubes

        This method is responsible to actually create the
        fluxcube image. Other internal methods as well as
        methods of aother classes are successively
        called to create the fluxcubes associated to the
        grism images listed in the header of the
        multidrizzled grism image.
        """
        import os
        import os.path

        # create the list of fluxcube instances that
        # will be created
        self.fcube_list.extend(self._fill_fcubelist(self.grism_image))

        # prepare the direct images:
        for fimage in self.filter_images:
            fimage.transform_toflux(self.segm_image)

        # adjust the image headers
        self._trim_imheaders(self.segm_image, self.filter_images)

        for fcube in self.fcube_list:
            res = fcube.create_fitscube(self.segm_image, self.filter_images,
                                        self.dim_info, self.interpol)
            if res:
                # something went wrong, delete the fcube
                if os.path.isfile(fcube.fcube_name):
                    os.unlink(fcube.fcube_name)
