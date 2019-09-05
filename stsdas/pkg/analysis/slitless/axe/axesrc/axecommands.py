"""
$Revision: 1.2 $ $Date: 2010/01/18 11:17:42 $
Author: Martin Kuemmel (mkuemmel@stecf.org)
Affiliation: Space Telescope - European Coordinating Facility
WWW: http://www.stecf.org/software/slitless_software/axesim/
"""
from __future__ import absolute_import, print_function
import os
import os.path
import sys
import subprocess

from .axeerror import aXeSIMError
from .axeutils import *

class DispImator(object):
    """
    Class to create a dispersed image
    """
    def __init__(self, dummyImages, configfile, simobjects, lambda_psf=None,
                 model_spectra=None, model_images=None):
        """
        Initializer for the class

        @param dummyImages: dummy image structure
        @type dummyImages: DummyImages()
        @param configfile: name of the aXe configuration file
        @type configfile: string
        @param simobjects: name of the model object table
        @type simobjects: string
        @param lambda_psf: reference wavelength for the psf
        @type lambda_psf: float
        @param model_spectra: name of the model spectra file
        @type model_spectra: string
        @param model_images: name of the model image file
        @type model_images: string
        """
        # save the naked name of the grism image
        self.grismname  = os.path.basename(dummyImages.griname)

        # check whether there is a direct image
        if dummyImages.dirname != None:
            # save the direct image name
            self.dirname = os.path.basename(dummyImages.dirname)
        else:
            # set the direct image name to 'None'
            self.dirname = None

        # save all other input to class variables
        self.configfile    = configfile
        self.iolname       = simobjects
        self.model_spectra = model_spectra
        self.model_images  = model_images
        self.lambda_psf    = lambda_psf

        # check whether model images are given
        # append the file name to the list
        if self.model_images != None:
            self.cont_model='direct'
        else:
            self.cont_model='gauss'


    def run(self, silent=True):
        """
        Generates a simulated dispersed image

        The method executes the series of aXe tasks necessary to generate
        a simulated dispersed image. The input from the class data is
        supplemented with default values.

        @param silent: boolean for silent mode
        @type silent: boolean
        """
        from . import axetasks

        # run SEX2GOL
        print('Running task "sex2gol" ...', end=' ')
        sys.stdout.flush()
        axetasks.sex2gol(grism=self.grismname, config=self.configfile, in_sex=self.iolname,
                         use_direct=True, direct=self.dirname, silent=silent)
        print(' Done')


        # run GOL2AF
        print('Running task "gol2af"  ...', end=' ')
        axetasks.gol2af(grism=self.grismname, config=self.configfile, orient=1,
                        slitless_geom=1, silent=silent)
        print(' Done')

        # run PETCONT
        print('Running task "petcont" ...', end=' ')
        sys.stdout.flush()
        axetasks.petcont(grism=self.grismname, config=self.configfile, cont_model=self.cont_model, spec_models=self.model_spectra,
                         object_models=self.model_images, lambda_psf=self.lambda_psf, no_pet=True, silent=silent)
        print(' Done')

    def mopup(self):
        """
        Deleting GOL and OAF files
        """
        import shutil

        # get the root name of the dispersed image
        pos = self.grismname.rfind('.fits')
        root_name   = self.grismname[:pos]

        # delete the GOL, the OAF and the PET
        result_cat = getOUTPUT(root_name   + '_2.cat')
        if os.path.isfile(result_cat):
            os.unlink(result_cat)
        result_oaf = getOUTPUT(root_name   + '_2.OAF')
        if os.path.isfile(result_oaf):
            os.unlink(result_oaf)

class DirImator(object):
    """
    Class to create a direct image
    """
    def __init__(self, dummyImages, configfile, simobjects, tpass_direct,
                 model_spectra=None, model_images=None, tel_area=None):
        """
        Initializer for the class

        @param dummyImages: dummy image structure
        @type dummyImages: DummyImages()
        @param configfile: name of the aXe configuration file
        @type configfile: string
        @param simobjects: name of the model object table
        @type simobjects: string
        @param tpass_direct: name of the total passband file
        @type tpass_direct: string
        @param model_spectra: name of the model spectra file
        @type model_spectra: string
        @param model_images: name of the model image file
        @type model_images: string
        @param tel_area: the collecting area of the telescope
        @type tel_area: float
        """
        # save the naked name of the direct image
        self.dirname = os.path.basename(dummyImages.dirname)

        # save all other input to local variables
        self.configfile    = configfile
        self.iolname       = simobjects
        self.tpass_direct  = tpass_direct
        self.model_spectra = model_spectra
        self.model_images  = model_images
        self.tel_area      = tel_area


    def run(self, silent=True):
        """
        Generates a simulated direct image

        The method executes the series of aXe tasks necessary to generate
        a simulated direct image.

        @param silent: boolean for silent mode
        @type silent: boolean
        """
        from . import axetasks

        # run SEX2GOL
        print('Running task "sex2gol"  ...', end=' ')
        sys.stdout.flush()
        axetasks.sex2gol(grism=self.dirname, config=self.configfile, in_sex=self.iolname,
                         use_direct=False, silent=silent)
        print(' Done')

        # run GOL2AF
        print('Running task "gol2af"   ...', end=' ')
        sys.stdout.flush()
        axetasks.gol2af(grism=self.dirname, config=self.configfile, silent=silent)
        print(' Done')

        # run DIRIMAGE
        print('Running task "dirimage" ...', end=' ')
        sys.stdout.flush()
        axetasks.axedirim(dirname=self.dirname, config=self.configfile, tpass_direct=self.tpass_direct,
                          model_spectra=self.model_spectra, model_images=self.model_images,
                          tel_area=self.tel_area, silent=silent)
        print(' Done')

    def mopup(self):
        """
        Deleting GOL and OAF files
        """
        import shutil

        # get the root name of the dispersed image
        pos = self.dirname.rfind('.fits')
        root_name   = self.dirname[:pos]

        # delete the GOL, the OAF and the PET
        result_cat = getOUTPUT(root_name   + '_2.cat')
        if os.path.isfile(result_cat):
            os.unlink(result_cat)
        result_oaf = getOUTPUT(root_name   + '_2.OAF')
        if os.path.isfile(result_oaf):
            os.unlink(result_oaf)


class DummyExtractor(object):
    """
    Class to make a dummy extraction
    """
    def __init__(self, dummyImages, grism_image, configfile, simobjects,
                 bck_flux, extrfwhm=3.0, orient=True, slitless_geom=True,
                 adj_sens=True, lambda_mark=None):
        """
        Initializes the class

        @param dummyImages: dummy image structure
        @type dummyImages: DummyImages()
        @param grism_image: the simulated grism image name
        @type grism_image: string
        @param configfile: name of the aXe configuration file
        @type configfile: string
        @param simobjects: name of the model object table
        @type simobjects: string
        @param bck_flux: backround-flux or image
        @type bck_flux: float/string
        @param extrfwhm: multiplier for extraction width
        @type extrfwhm: float
        @param orient: flag for tilted extraction
        @type orient: boolean
        @param slitless_geom: flag for slitless optimized extraction
        @type slitless_geom: boolean
        @param adj_sens: flag for adjusted flux conversion
        @type adj_sens: boolean
        @param lambda_mark: wavelength to apply cutoff magnitudes
        @type lambda_mark: float
        """
        # save the direct image name
        self.direct_image    = os.path.basename(dummyImages.dirname)
        self.simul_grisim    = os.path.basename(grism_image)
        self.dispersed_image = None

        # save all other input to local variables
        self.configfile    = configfile
        self.iolname       = simobjects
        self.bck_flux      = bck_flux
        self.extrfwhm      = extrfwhm
        self.orient        = orient
        self.slitless_geom = slitless_geom
        self.adj_sens      = adj_sens
        self.lambda_mark   = lambda_mark

        # check whether everything
        # is where it is supposed to be
        self._check_files()

    def _check_files(self):
        """
        Checks the existence of the input files
        """
        # check the direct image
        if not os.path.isfile(getIMAGE(self.direct_image)):
            err_msg = '\nThe direct image is not available: ' + getIMAGE(self.direct_image)
            raise aXeSIMError(err_msg)

        # check the configuration file
        if not os.path.isfile(getCONF(self.configfile)):
            err_msg = '\nThe configuration file is not available: ' + getCONF(self.configfile)
            raise aXeSIMError(err_msg)

        # check the simulated grism image
        if not os.path.isfile(getOUTSIM(self.simul_grisim)):
            err_msg = '\nThe grism image is not available: ' + getOUTSIM(self.simul_grisim)
            raise aXeSIMError(err_msg)

        # check the IOL
        if not os.path.isfile(self.iolname):
            err_msg = '\nThe Input Object List is not available: ' + self.iolname
            raise aXeSIMError(err_msg)

        try:
            float(self.bck_flux)
        except ValueError:
            # check the background image
            if not os.path.isfile(getCONF(self.bck_flux)):
                err_msg = '\nThe background imagage is not available: ' + getCONF(self.bck_flux)
                raise aXeSIMError(err_msg)

    def _decorate_PET(self):
        """
        Write the 'CONTAM'-keyword into the PET

        The method determines the name of the PET and
        sets the contamination keyword in the zero-extension header
        """
        from astropy.io import fits as pyfits

        # get the root name of the dispersed image
        root_name   = self.dispersed_image.split('.fits')[0]

        # compose the name of the PET
        result_pet = getOUTPUT(root_name   + '_2.PET.fits')

        # open the PET
        pet_fits = pyfits.open(result_pet, 'update')

        # update the PET header
        pet_fits[0].header['CONTAM'] = ( 'GEOM', 'dummy flag - no quantitative contamination')

        # close and out
        pet_fits.close()

    def prepare_extraction(self):
        """
        Prepares the aXe extraction

        The module does some preparatory stuff before the extraction
        can start. This includes copying the simulated dispersed image
        to AXE_IMAGE_PATH and subtracting the background on this copy.
        """
        import shutil
        from pyraf import iraf

        # give brief feedback
        print('Dummy extraction on the dispersed image:')
        sys.stdout.flush()

        # get a random filenames
        tmpfile1 = get_random_filename('t', '.fits')
        tmpfile2 = get_random_filename('t', '.fits')

        # copy the grism image to AXE_IMAGE_PATH
        shutil.copy(getOUTSIM(self.simul_grisim), getIMAGE(tmpfile1))

        # subtract the background from
        # the grism image
        expression="(a - b)"
        iraf.imexpr(expr=expression, output=tmpfile2,
                    a=getIMAGE(tmpfile1)+'[SCI]', b=self.bck_flux, Stdout=1)

        # copy the background subracted image to the grism image, sci-extension
        iraf.imcopy(input=tmpfile2, output=getIMAGE(tmpfile1)+'[SCI,overwrite]', Stdout=1)

        # delete the background subtracted
        # tmp image
        os.unlink(tmpfile2)

        # store the name of the background
        # subtracted grism image
        self.dispersed_image = tmpfile1

    def mopup(self):
        """
        Deleting tmp-files, copying SPC's, STP's
        """
        import shutil

        # get the root name of the dispersed image
        root_name   = self.dispersed_image.split('.fits')[0]

        #  get the root name of the simulated image
        result_root = self.simul_grisim.split('.fits')[0]

        # move and rename the SPC-file
        out_spc    = getOUTPUT(root_name   + '_2.SPC.fits')
        result_spc = getOUTSIM(result_root + '_2.SPC.fits')
        shutil.move(out_spc, result_spc)

        # move and rename the STP-file
        out_stp    = getOUTPUT(root_name   + '_2.STP.fits')
        result_stp = getOUTSIM(result_root + '_2.STP.fits')
        shutil.move(out_stp, result_stp)

        # delete the background subtracted
        # grism image
        os.unlink(getIMAGE(self.dispersed_image))

        # delete the GOL, the OAF and the PET
        result_cat = getOUTPUT(root_name   + '_2.cat')
        if os.path.isfile(result_cat):
            os.unlink(result_cat)
        result_oaf = getOUTPUT(root_name   + '_2.OAF')
        if os.path.isfile(result_oaf):
            os.unlink(result_oaf)
        result_pet = getOUTPUT(root_name   + '_2.PET.fits')
        if os.path.isfile(result_pet):
            os.unlink(result_pet)


    def run(self, silent=True):
        """
        Generates a simulated dispersed image

        The method executes the series of aXe tasks necessary to generate
        a simulated dispersed image. The input from the class data is
        supplemented with default values.

        @param silent: boolean for silent mode
        @type silent: boolean
        """
        from . import axetasks

        # run SEX2GOL
        print('Running task "sex2gol" ...', end=' ')
        sys.stdout.flush()
        axetasks.sex2gol(grism=self.dispersed_image, config=self.configfile, in_sex=self.iolname,
                         use_direct=True,  direct=self.direct_image, silent=silent)
        print(' Done')

        # run GOL2AF
        print('Running task "gol2af"  ...', end=' ')
        sys.stdout.flush()
        axetasks.gol2af(grism=self.dispersed_image, config=self.configfile, mfwhm=self.extrfwhm,
                        orient=self.orient, slitless_geom=self.slitless_geom,
                        lambda_mark=self.lambda_mark, silent=silent)
        print(' Done')

        # run AF2PET
        print('Running task "af2pet"  ...', end=' ')
        sys.stdout.flush()
        axetasks.af2pet(grism=self.dispersed_image, config=self.configfile, silent=silent)
        print(' Done')

        #-----------------------------------------------
        # set the contamination keyword
        #
        # Running PECONT, is, since we are doing
        # a simulation, not very reasonable.
        # However PET2SPC complains if the contmaintion
        # keyword is not set. A solution is just to set
        # the geometrical contamination keyword to make
        # the warning in PET2SPC disappear.
        self._decorate_PET()
        #-----------------------------------------------

        # run PET2SPC
        print('Running task "pet2spc" ...', end=' ')
        sys.stdout.flush()
        axetasks.pet2spc(grism=self.dispersed_image, config=self.configfile,
                         adj_sens=self.adj_sens, silent=silent)
        print(' Done')

        # run STAMPS
        print('Running task "stamps"  ...', end=' ')
        sys.stdout.flush()
        axetasks.stamps(grism=self.dispersed_image, config=self.configfile,
                        sampling='rectified', silent=silent)
        print(' Done')
