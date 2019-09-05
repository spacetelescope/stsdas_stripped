"""
$Revision: 1.2 $ $Date: 2010/05/18 07:56:00 $
Author: Martin Kuemmel (mkuemmel@stecf.org)
Affiliation: Space Telescope - European Coordinating Facility
WWW: http://www.stecf.org/software/slitless_software/axesim/
"""
from __future__ import absolute_import, print_function

import os
import os.path
import sys
import string
from astropy.io import fits as pyfits
from axe import axe_asciidata

from .axeerror import aXeSIMError
from .axeutils import *

class ArtImaList(object):
    """
    Class for the image list
    """
    def __init__(self, inlist):
        """
        Initializer for the class

        @param inlist: name of list with the fits images
        @type inlist: string
        """
        # initialize the list of spectra
        self._imalist = []

        # load the list with input images
        ilist = axe_asciidata.open(inlist)

        # go over the image names
        for item in ilist[0]:

            # check whether the file is there
            if not os.path.isfile(getSIMDATA(item)):
                error_message = '\nDid not find image: ' + str(getSIMDATA(item)) + ' !!'
                raise aXeSIMError(error_message)

            # apend the fits to the liast
            self._imalist.append(ArtImage(getSIMDATA(item.strip())))


    def tofits(self, fitsname, indata_copy=0):
        """
        Converts and stores the spectra in a fits file

        Converts all images stored in the class instance to a
        multi-extension fits image with a name gvien as input.
        A flagg indicates whether, besides the normal output to
        AXE_OUTSIM_PATH, a copy to AXE_IMAGE_PATH is desired.

        @param fitsname: name for the MEX fits file
        @type fitsname: string
        @param indata_copy: flag to save also a copy
        @type indata_copy:int
        """
        # create a HDU list
        hdulist = pyfits.HDUList()

        # create an empty primary HDU
        phdu = pyfits.PrimaryHDU()

        # put the primary to the list
        hdulist.append(phdu)

        # give a linefeed
        print('')

        # check whther the fitsname
        # ends with '.fits'
        pos = fitsname.rfind('.fits')
        if not (pos > -1 and pos == len(fitsname) - len('.fits')):
            fitsname += '.fits'

        # initialize an index
        index = 0

        # go over all spectra
        for ima in self._imalist:

            # enhance the counter
            index += 1

            # append the the image HDU
            # to the output fits
            hdulist.append(ima.imgHDU)

            # print what you do on the screen
            print('Adding: %s to %s, ext: %s' % (os.path.basename(ima.filename), fitsname, str(index)))

        # delete older versions
        # of the fits name
        if os.path.isfile(getOUTSIM(fitsname)):
            os.unlink(getOUTSIM(fitsname))

        print('\nWriting images to file: ', getOUTSIM(fitsname), ' ... ', end=' ')
        # write it to fits
        hdulist.writeto(getOUTSIM(fitsname))
        # give an end notice
        print('Done')

        # check whether a copy
        # is needed at AXE_IMAGE_PATH directory
        if indata_copy:
            # delete older versions
            # of the fits name
            if os.path.isfile(getIMAGE(fitsname)):
                os.unlink(getIMAGE(fitsname))

            print('Writing images to file: ', getIMAGE(fitsname), ' ... ', end= ' ')
            # write it to fits
            hdulist.writeto(getIMAGE(fitsname))
            # give an end notice
            print('Done')

        # add an extra linefeed
        print('')


class ArtImage(object):
    """
    Class for one image template
    """
    def __init__(self, filename):
        """
        Initializer for the class

        @param filename: name of the spectrum
        @type filename: string
        """
        # store the filename
        self.filename = filename

        # define the image name,
        # which is the file name until
        # the last '.', e.g. 'psf.fits' --> 'psf'
        self.imaname = os.path.basename(filename[:filename.rfind('.')])

        # create an image HDU from the fits
        self.imgHDU = self._makeImgHDU(self.filename, self.imaname)


    def _makeImgHDU(self, filename, imaname):
        """
        Extract and return the first non-empty image HDU from the fits

        The method opens a fits image and goes along its extension.
        The first extension with a non-zero data part is returned
        after updating the header with a given image name.

        @param filename: name of the fits file
        @type filename: string
        @param imaname: name of the image
        @type imaname: string

        @return: the image hdu
        @rtype: hdu()
        """
        # intialize the image HDU
        imgHDU = None

        # open the fits
        fitsHDU = pyfits.open(filename, 'update')

        # initialize an index
        index = 0

        # go over all HDU's in the fits
        for HDU in fitsHDU:

            # check for an existing data HDU
            if HDU.data != None:

                # write the image name to a keyword
                HDU.header['IMANAME'] =  imaname

                # normalize the data
                normData = self._norm_data(HDU.data)

                # create an image HDU
                imgHDU =  pyfits.ImageHDU(normData, header=HDU.header,name=imaname)
                #print  'File: ', os.path.basename(filename), ' ext: ', str(index),  ' loaded'
                print('File %s, ext: %s loaded' % (filename, str(index)))

                # exit after taking the first HDU with data
                break

        # close the fits file
        fitsHDU.close()

        # return the image HDU
        return imgHDU

    def _norm_data(self, Imgdata):
        """
        Normalize the image data

        The method normalizes the image data. It uses
        methods of the data class (numpy or numarray).

        @param Imgdata: the image data
        @type Imgdata: <ImgData()>

        @return: the normalized image data
        @rtype: <ImgData()>

        """
        # get the sum of all pixels
        cts_sum = Imgdata.sum()

        # create the  normalized image data
        normData =  Imgdata / cts_sum

        # return the normalized image data
        return normData
