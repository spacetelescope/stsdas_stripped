"""
$Revision: 1.1 $ $Date: 2010/01/18 09:16:15 $
Author: Martin Kuemmel (mkuemmel@stecf.org)
Affiliation: Space Telescope - European Coordinating Facility
WWW: http://www.stecf.org/software/slitless_software/axesim/
"""
from __future__ import absolute_import, print_function

import os
import os.path
import sys

from pyraf import iraf
from iraf import artdata

from .axeerror import aXeSIMError
from .axeutils import *

class RealWorld(object):
    """
    Class for to add poisson noise
    """
    def __init__(self, image_name, extname='0', exptime=1.0, bck_flux=0.0,
                 rdnoise=0.0, instrument=None):
        """
        Initializes the class

        The various input for the noise model is passed as
        parameters. Reasonable defaults are defined as well.
        'None' as input is converted to the corresponding default.

        @param image_name: the name of the input image
        @type image_name: string
        @param extname: the extension to use
        @type extname: string
        @param exptime: the exposure time
        @type exptime: float
        @param bck_flux: the background flux
        @type bck_flux: float
        @param rdnoise: the readout noise
        @type rdnoise: float
        """
        # check whether the image exists
        if not os.path.isfile(image_name):
            err_msg = '\nImage: "'+image_name+'" does not exist!'
            raise aXeSIMError(err_msg)

        # save the parameters
        self.image_name = image_name
        self.extname    = extname
        self.exptime    = exptime

        # determine and store the image dimensions
        self.dimension = self._get_dimension(self.image_name, self.extname)

        # store the parameter for
        # the default background;
        # convert 'None' to default
        if bck_flux != None:
            self.bck_flux    = bck_flux
        else:
            self.bck_flux   = 0.0

        # store the parameter for
        # the CR-boolean
        # convert 'None' to default
        if rdnoise != None:
            self.rdnoise    = rdnoise
        else:
            self.rdnoise    = 0.0

        # store the parameter for
        # instrument keyword
        # convert 'None' to default
        if instrument != None:
            self.instrument = instrument
        else:
            self.instrument    = 'aXeSIM'

    def _set_keywords(self):
        """
        Set header kewords in output image

        The method sets header keywords in the zero extension
        header of the output image.
        """
        from astropy.io import fits as pyfits

        # open the fits image
        inima = pyfits.open(self.image_name, 'update')

        # write the instrument name in the header
        inima[0].header['INSTRUME'] = ( self.instrument, 'instrument name')

        # write the exposure time in the header;
        # 'None' is converted to 1.0
        if self.exptime != None:
            inima[0].header['EXPTIME'] = ( self.exptime, 'exposure time')
        else:
             inima[0].header['EXPTIME'] = ( 1.0, 'default exposure time')

        # close the image
        inima.close()

    def _compose_multiext_image(self, sci_ext, err_ext, dq_ext, del_input=1):
        """
        Compose the multi-extension image from individual layers

        @param sci_ext: the science extension image
        @type sci_ext: string
        @param err_ext: the error extension image
        @type err_ext: string
        @param dq_ext: the dq extension image
        @type dq_ext: string
        """
        # set the various keywords
        self._set_keywords()

        # put the noisy stuff on the input image, replacing the original first extension
        iraf.imcopy(input=sci_ext, output=self.image_name+'['+str(self.extname)+',overwrite]', Stdout=1)
        iraf.imcopy(input=err_ext, output=self.image_name+'[ERR, append]', Stdout=1)
        iraf.imcopy(input=dq_ext,  output=self.image_name+'[DQ, append]', Stdout=1)

        # delete the input images,
        # if reuqested
        if del_input:
            os.unlink(sci_ext)
            os.unlink(err_ext)
            os.unlink(dq_ext)

    def _get_dimension(self, image_name, extname):
        """
        Get the image dimension

        @param image_name: the name of the image
        @type image_name: string
        @param extname: the extension name
        @type extname: string

        @return: the image-dimension (yaxis, xaxis)
        @rtype: (int, int)
        """
        from astropy.io import fits as pyfits

        # open the fits
        f_img = pyfits.open(image_name, 'readonly')

        # extract the image dimension
        dimension = f_img[extname].data.shape

        # close the image
        f_img.close()

        # return the dimension
        return dimension


    def _make_real_sciimage(self, in_image, bck_flux, exptime):
        """
        Create the science extension

        Starting from a simulated image in [e/s], the module adds
        background and scales to [e]. The output image is returned.

        @param in_image: the simulated image
        @type in_image: string
        @param bck_flux: background flux [e/s]
        @type bck_flux: float
        @param exptime: the exposure time
        @type exptime: float
        """
        # get a random filename
        tmpfile1 = get_random_filename('t', '.fits')

        # add background; scale by exptime
        expression="(a + b)*c"
        iraf.imexpr(expr=expression, output=tmpfile1,
                    a=in_image, b=bck_flux, c=exptime, Stdout=1)

        # return the image name
        return tmpfile1

    def _add_noise(self, in_image):
        """
        Add noise to an image

        The module adds noise to an image in [e].

        @param in_image: the input image
        @type in_image: string
        """
        # unlearn some iraf tasks
        iraf.unlearn('mknoise')

        # add noise and background to the image
        iraf.mknoise(input=in_image, background=0.0, gain=1.0,
                     rdnoise=self.rdnoise, poisson="YES", seed="INDEF")

    def _compute_err_ext(self, sci_ext):
        """
        Compute the error image for a science image

        For a image in [e], the module computes the associated
        error image assuming a simple noise model with photon shot noise
        and readout noise.

        @param sci_ext: the input image
        @type sci_extsci_ext: string
        """
        # unlearn some iraf tasks
        iraf.unlearn('imexpr')
        # get a random filename
        tmpfile1 = get_random_filename('t', '.fits')

        # compute the error array
        expression="sqrt(a + b*b)"
        iraf.imexpr(expr=expression, output=tmpfile1, a=sci_ext, b=self.rdnoise, Stdout=1)

        return tmpfile1

    def _scale_image(self, in_image):
        """
        Converts from [e] to [e/s]

        @param in_image: the input image
        @type in_image: string
        """
        # unlearn some iraf tasks
        iraf.unlearn('imarith')

        # get a random filename
        tmpfile1 = get_random_filename('t', '.fits')

        # divide by exposure time
        iraf.imarith(operand1=in_image, op="/", operand2=self.exptime, result=tmpfile1)

        # delete the old image;
        # rename the new to the old name
        os.unlink(in_image)
        iraf.imrename(oldnames=tmpfile1, newnames=in_image)

    def _make_dq_extension(self):
        """
        Creates an empty dq-extension image
        """
        # unlearn some iraf tasks
        iraf.unlearn('mkpattern')

        # get a random filename
        tmpfile1 = get_random_filename('t', '.fits')

        iraf.mkpattern(input=tmpfile1, output="", pattern="constant", option="replace",
                       v1=0.0, v2=0.0, size=1, title="", pixtype="integer",
                       ndim=2, ncols=self.dimension[1], nlines=self.dimension[0],
                       n3=1, n4=1, n5=1, n6=1, n7=1, header="")
        return tmpfile1

    def _make_const_image(self, value):
        """
        Creates a constant image
        """
        # unlearn some iraf tasks
        iraf.unlearn('mkpattern')

        # get a random filename
        tmpfile1 = get_random_filename('t', '.fits')

        iraf.mkpattern(input=tmpfile1, output="", pattern="constant", option="replace",
                       v1=value, v2=0.0, size=1, title="", pixtype="real",
                       ndim=2, ncols=self.dimension[1], nlines=self.dimension[0],
                       n3=1, n4=1, n5=1, n6=1, n7=1, header="")
        return tmpfile1

    def make_real(self):
        """
        Create a 'natural' image

        Depending on the class data, method adds background and noise
        in ordr to create a 'natural' image from the plain,
        simulated image.
        """
        print('\nCompleting image ...', end=' ')
        sys.stdout.flush()
        if self.exptime != None:
            self.make_real_exptime()
        else:
            self.make_real_noexp()
        print(' Done\n')

    def make_real_noexp(self):
        """
        Create a 'natural' image

        The method creates a natural image without noise
        """
        # get the science image plus background
        # in electrons
        sci_scaled = self._make_real_sciimage(self.image_name+'['+str(self.extname)+']',
                                              self.bck_flux, 1.0)
        # compute the error extension
        err_scaled = self._make_const_image(self.rdnoise)

        # make a dq-extension
        dq_ext = self._make_dq_extension()

        # compose the final multi-extension image
        self._compose_multiext_image(sci_scaled, err_scaled, dq_ext, 1)

    def make_real_exptime(self):
        """
        Create a 'natural' image

        The method creates a natural image with noise
        """
        # get the science image plus background
        # in electrons
        sci_scaled = self._make_real_sciimage(self.image_name+'['+str(self.extname)+']',
                                              self.bck_flux, self.exptime)

        # add readout and poisson errors
        self._add_noise(sci_scaled)

        # compute the error extension
        err_scaled = self._compute_err_ext(sci_scaled)

        # check whether the exposure time is non-zero
        if self.exptime != 0.0:
            # scale both extensions
            # by the exposure time
            self._scale_image(sci_scaled)
            self._scale_image(err_scaled)

        # make a dq-extension
        dq_ext = self._make_dq_extension()

        # compose the final multi-extension image
        self._compose_multiext_image(sci_scaled, err_scaled, dq_ext, 1)
