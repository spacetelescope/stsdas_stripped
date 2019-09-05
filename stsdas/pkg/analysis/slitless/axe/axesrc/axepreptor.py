"""
$Revision: 2.0 $ $Date: 2011-05-10 11:36:12 $
Author: Martin Kuemmel
Affiliation: Space Telescope - European Coordinating Facility
"""
from __future__ import absolute_import, print_function

__author__ = "Martin Kuemmel <mkuemmel@eso.org>"
__date__ = "$Date: 2011-05-10 11:36:12 $"
__version__ = "$Revision: 2.0 $"
__credits__ = """This software was developed by the ACS group of the Space Telescope -
European Coordinating Facility (ST-ECF). The ST-ECF is a department jointly
run by the European Space Agency and the European Southern Observatory.
It is located at the ESO headquarters at Garching near Munich. The ST-ECF
staff supports the European astronomical community in exploiting the research
opportunities provided by the earth-orbiting Hubble Space Telescope.

H. Bushouse, STScI, 2011-05-10, version 2.0: Major rewrite of some routines to
remove all calls to IRAF tasks, such as imrename, imcopy, imarith, and
imexpression. All such calls have been replaced with equivalent Python and
PyFITS file utilities and numpy array arithmetic.
"""
"""
This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
"""
from . import axeutils
from .axeerror import aXeError

class aXePrepArator(object):
    def __init__(self, grisim, objcat, dirim, config, dmag, **params):
        """
        Initializes the object
        """
        # store the input
        self.grisim = grisim
        self.objcat = objcat
        self.dirim  = dirim
        self.config = config
        self.dmag   = dmag

        self.params = params

        # store the master background
        if 'master_bck' in params:
            self.master_bck = params['master_bck']

    def _is_nicmos_data(self):
        """
        Check whether the data comes from NICMOS
        """
        from astropy.io import fits as pyfits

        # open the image
        fits_img = pyfits.open(axeutils.getIMAGE(self.grisim), 'readonly')

        # open the image
        if 'INSTRUME' in fits_img[0].header:

            # check whether the instrument IS NICMOS
            instrument = fits_img[0].header['INSTRUME']
            if instrument.find('NICMOS') > -1:

                # close image and return True
                fits_img.close()
                return 1

        # close image and return False
        fits_img.close()
        return 0

    def _is_wfc3ir_data(self):
        """
        Check whether the data comes from WFC3 IR channel
        """
        from astropy.io import fits as pyfits

        # open the image
        fits_img = pyfits.open(axeutils.getIMAGE(self.grisim), 'readonly')

        # open the image
        if 'INSTRUME' in fits_img[0].header:

            # check whether the instrument IS NICMOS
            instrument = fits_img[0].header['INSTRUME']
            if instrument.find('WFC3') > -1:

                # check whether the detector IS IR
                if 'DETECTOR' in fits_img[0].header and fits_img[0].header['DETECTOR'].find('IR') > -1:
                    # close image and return True
                    fits_img.close()
                    return 1

        # close image and return False
        fits_img.close()
        return 0

    def _make_mask(self):
        """
        Make the background mask file
        """
        from . import axetasks

        # set the use_direct flag
        # NOTE: the flag is only useful
        #       for the C-version, the python
        #       version does not need it!
        if self.dirim != None:
            use_direct = True
        else:
            use_direct=False

        # run SEX2GOL
        axetasks.sex2gol(grism=self.grisim, config=self.config, in_sex=self.objcat, use_direct=use_direct, direct=self.dirim,
                         dir_hdu=None, spec_hdu=None, out_sex=None)

        # run GOL2AF
        axetasks.gol2af(grism=self.grisim, config=self.config, mfwhm=self.params['mfwhm'], back=False,
                        orient=True, slitless_geom=True, exclude=False, lambda_mark=None, dmag=self.dmag,
                        out_af=None, in_gol=None)

        #  run BACKEST
        axetasks.backest(grism=self.grisim, config=self.config, np=0, interp=-1, niter_med=None,
                         niter_fit=None, kappa=None, smooth_length=None, smooth_fwhm=None,
                         old_bck=False, mask=True, in_af=None, out_bck=None)

    def _subtract_sky(self, ext_info):
        """
        Make a classical background subtraction
        """
        import os, shutil

        from pyraf import iraf
        from iraf import stsdas

        from astropy.io import fits as pyfits
        import stsci.imagestats as imagestats

        # get the axe names
        axe_names = axeutils.get_axe_names(self.grisim, ext_info)

        msk_image_sc = axe_names['MSK']   + '[SCI]'

        # check for a previous background subtraction
        fits_img  = pyfits.open(axeutils.getIMAGE(self.grisim), 'readonly')
        fits_head = fits_img[ext_info['fits_ext']].header
        npix = int(fits_head['NAXIS1']) * int(fits_head['NAXIS1'])

        if 'AXEPRBCK' in fits_head:
            # warn that this is the second time
            print('WARNING: Image %25s seems to be already background subtracted!' % axeutils.getIMAGE(self.grisim))

        # close the fits
        fits_img.close()

        # Compute the ratio of the grism SCI image to the background image
        sci_file = pyfits.open(axeutils.getIMAGE(self.grisim),'readonly')
        sci_data = sci_file['SCI',ext_info['ext_version']].data
        bck_file = pyfits.open(axeutils.getCONF(self.master_bck),'readonly')
        bck_data = bck_file[0].data
        sci_data /= bck_data

        # Flag pixels in the ratio image based on the grism image DQ array
        dq_data = sci_file['DQ',ext_info['ext_version']].data
        sci_data[dq_data > 0.5] = -1.0e10

        # Flag pixels in the ratio image based on the grism image MSK file
        msk_file = pyfits.open(axeutils.getOUTPUT(msk_image_sc.split("[")[0]), 'readonly')
        msk_data = msk_file['SCI'].data
        sci_data[msk_data < -900000] = -1.0e10

        # Flag pixels in the background image based on the grism image DQ
        # and MSK file
        bck_data[dq_data > 0.5] = -1.0e10
        bck_data[msk_data < -900000] = -1.0e10

        # Compute stats for the ratio image
        stats = imagestats.ImageStats(sci_data[sci_data > -1.0e9],
                                      fields='midpt,stddev,npix', lower=None,
                                      upper=None, nclip=3, lsig=3.0, usig=3.0,
                                      binwidth=0.01)
        flt_ave = stats.midpt
        flt_std = stats.stddev
        flt_npx = stats.npix
        frac_pix = float(flt_npx)/float(npix)

        # Compute stats for the background image
        stats = imagestats.ImageStats(bck_data[bck_data > -1.0e9],
                                      fields='midpt,stddev,npix', lower=None,
                                      upper=None, nclip=3, lsig=3.0, usig=3.0,
                                      binwidth=0.01)
        mst_ave = stats.midpt
        mst_std = stats.stddev
        mst_npx = stats.npix

        sci_file.close()
        bck_file.close()
        msk_file.close()

        # Subtract the scaled background from the grism image
        sci_file = pyfits.open(axeutils.getIMAGE(self.grisim),'update')
        bck_file = pyfits.open(axeutils.getCONF(self.master_bck),'readonly')
        sci_file['SCI',ext_info['ext_version']].data -= flt_ave*bck_file[0].data
        sci_file.close()
        bck_file.close()

        # open the fits image ands isolate the correct extension
        grism_img    = pyfits.open(axeutils.getIMAGE(self.grisim), 'update')
        grism_header = grism_img[ext_info['fits_ext']].header

        # write some header iformation
        grism_header['SKY_SCAL'] = (float(flt_ave),  'scaling value for the master background')
        grism_header['SKY_MAST'] = (float(mst_ave),  'average value of the master background')
        grism_header['SKY_IMG'] = (self.master_bck, 'name of the master background image')
        grism_header['F_SKYPIX'] = (frac_pix,        'fraction of pixels used for scaling')
        grism_header['AXEPRBCK'] = ('Done',          'flag that background subtraction was done')

        # save the image
        grism_img.close()

        return 0

    def _subtract_nicsky(self, ext_info):
        """
        Special sky subtraction for NICMOS images
        """
        import os
        import os.path

        from pyraf import iraf
        from iraf import stsdas

        from astropy.io import fits as pyfits

        from . import axelowlev

        # get the axe names
        axe_names = axeutils.get_axe_names(self.grisim, ext_info)

        # check for a previous background subtraction
        fits_img  = pyfits.open(axeutils.getIMAGE(self.grisim), 'readonly')
        fits_head = fits_img[ext_info['fits_ext']].header
        npix = int(fits_head['NAXIS1']) * int(fits_head['NAXIS1'])

        if 'AXEPRBCK' in fits_head:
            # warn that this is the second time
            print('WARNING: Image %25s seems to be already background subtracted!' % axeutils.getIMAGE(self.grisim))

        # close the fits
        fits_img.close()

        # do the special background fitting for NICMOS
        if self.params['backped'] != None:
            nicback = axelowlev.aXe_NICBACK(self.grisim, self.config, self.master_bck, self.params['backped'])
        else:
            nicback = axelowlev.aXe_NICBACK(self.grisim, self.config, self.master_bck)
        nicback.runall()
        del nicback

        # check whether the background image exists
        if not os.path.isfile(axeutils.getOUTPUT(axe_names['NBCK'])):
            err_msg = 'The background image: %s does NOT exist!' % axeutils.getOUTPUT(axe_names['NBCK'])
            raise aXeError(err_msg)

        # Subtract the scaled background image from the grism image
        sci_file = pyfits.open(axeutils.getIMAGE(self.grisim), 'update')
        bck_file = pyfits.open(axeutils.getOUTPUT(axe_names['NBCK']),'readonly')
        sci_file['SCI',ext_info['ext_version']].data -= bck_file[1].data
        sci_file.close()
        bck_file.close()

        # open the background image
        fits_img  = pyfits.open(axeutils.getOUTPUT(axe_names['NBCK']), 'readonly')
        fits_head = fits_img['BCK'].header

        # open the grism image and isolate the correct extension header
        grism_img    = pyfits.open(axeutils.getIMAGE(self.grisim), 'update')
        grism_header = grism_img[ext_info['fits_ext']].header

        if 'SKY_SCAL' in fits_head and 'F_SKYPIX' in fits_head:

            # transfer important keywords
            # to the grism image
            grism_header['SKY_SCAL'] = (float(fits_head['SKY_SCAL']), 'scaling value of background')
            grism_header['F_SKYPIX'] = (float(fits_head['F_SKYPIX']), 'fraction of pixels used for scaling')

        # close the fits again
        fits_img.close()

        # write some keywords
        grism_header['AXEPRBCK'] = ('Done', 'flag that background subtraction was done')
        grism_header['SKY_IMG'] =  (self.master_bck, 'name of the 1st master background image')
        if self.params['backped'] != None:
            grism_header['SKY_IMG2'] = (self.params['backped'], 'name of the 2nd master background image')

        # close the image again
        grism_img.close()

        return True

    def _subtract_wfc3irsky(self, ext_info):
        """
        Special sky subtraction for NICMOS images
        """
        import os
        import os.path

        from pyraf import iraf
        from iraf import stsdas

        from astropy.io import fits as pyfits

        from . import axelowlev

        # get the axe names
        axe_names = axeutils.get_axe_names(self.grisim, ext_info)

        # check for a previous background subtraction
        fits_img  = pyfits.open(axeutils.getIMAGE(self.grisim), 'readonly')
        fits_head = fits_img[ext_info['fits_ext']].header
        npix = int(fits_head['NAXIS1']) * int(fits_head['NAXIS1'])

        if 'AXEPRBCK' in fits_head:
            # warn that this is the second time
            print('WARNING: Image %25s seems to be already background subtracted!' % axeutils.getIMAGE(self.grisim))

        # close the fits
        fits_img.close()


        scalebck = axelowlev.aXe_SCALEBCK(self.grisim, axe_names['MSK'], self.config, self.master_bck)
        try:
            scalebck.runall()
        except aXeError:
            print("There was a problem with the background subtraction, continuing without it")
            return False

        # check whether the background image exists
        bckfilename=axeutils.getOUTPUT(axe_names['SGRI'])
        if not os.path.isfile(bckfilename):
            err_msg = 'The background image: %s does NOT exist!' % bckfilename
            raise aXeError(err_msg)

        # in case of a low  value, dont do the subtraction if less than 10%
        sky_frac=pyfits.getval(bckfilename, "FRACFIN",ext=0)

        if sky_frac < 0.1:
            #self._check_low_skyfrac(sky_frac)
            print("Low fraction of sky pixels found (<10%) continuing WITHOUT sky subtraction")
            return False

        # Subtract the scaled background image from the grism image
        sci_file = pyfits.open(axeutils.getIMAGE(self.grisim), 'update')
        bck_file = pyfits.open(bckfilename)
        sci_file['SCI',ext_info['ext_version']].data -= bck_file[1].data
        sci_file.close()
        bck_file.close()

        # open the background image
        fits_img  = pyfits.open(bckfilename, 'readonly')
        fits_head = fits_img[0].header

        # open the grism image and isolate the correct extension header
        grism_img    = pyfits.open(axeutils.getIMAGE(self.grisim), 'update')
        grism_header = grism_img[ext_info['fits_ext']].header

        # write some information into the
        # grism image header
        grism_header['AXEPRBCK'] = ('Done', 'flag that background subtraction was done')
        grism_header['SKY_IMG'] =  (self.master_bck, 'name of the 1st master background image')

        # write some scaling information into the header
        grism_header['F_SKYPIX'] =  (float(fits_head['FRACFIN']), 'fraction of pixels used for scaling')
        grism_header['SKY_CPS'] = (float(fits_head['SCALVAL']),  'scale used for master sky == sky value [cps]')

        # close the grism image
        # and the scaled image
        fits_img.close()
        grism_img.close()


        return True

    def _subtract_background(self, ext_info):
        """
        Determine and subtract the background
        """
        goodreturn=True

        # generate the mask image
        self._make_mask()

        # check whether we have NICMOS
        if self._is_nicmos_data():
            # make normal background subtraction
            goodreturn=self._subtract_nicsky(ext_info)

        elif self._is_wfc3ir_data():
            # make normal background subtraction
            goodreturn=self._subtract_wfc3irsky(ext_info)

        else:
            # make normal background subtraction
            goodreturn=self._subtract_sky(ext_info)

        if goodreturn :
            pstring = 'AXEPREP: Image %25s[SCI,%s] sky-subtracted.' % (self.grisim,str(ext_info['ext_version']))
            print(pstring)

    def _check_low_skyfrac(self, frac):
        """
        Check for a low fraction of background pixels
        """
        from pyraf import iraf
        from pyraf.irafpar import IrafPar,IrafParI,IrafParS, IrafParList, makeIrafPar

        print('')
        print('AXEPREP Image %s: only %.1f percent of the pixels we used in the background scaling!' % (self.grisim, frac*100.0))

        idec = IrafParS(['         Continue or quit? [(y)es/(q)uit] :(q)','string','h'],'whatever')
        idec.getWithPrompt()
        dec = idec.value.strip()
        if dec.upper() == 'Y':
            print('         Continue!')
            print('')
            return 1
        else:
            err_msg = 'AXEPREP: Not enough background pixels for image: %s!' % self.grisim
            raise aXeError(err_msg)

    def _check_second_normalization(self):
        """
        Check whether the data is already normalized
        """
        from pyraf import iraf
        from pyraf.irafpar import IrafPar,IrafParI,IrafParS, IrafParList, makeIrafPar

        print('')
        print('AXEPREP: Image %25s has just been normalized!' %self.grisim)

        idec = IrafParS(['         Normalize it again?[(y)es/(n)o/(q)uit] :(q)','string','h'],'whatever')
        idec.getWithPrompt()
        dec = idec.value.strip()
        if dec.upper() == 'Y':
            print('         Continue!')
            print('')
            return 1
        elif dec.upper() == 'N':
            print('         Not normalizing image: %s!' % self.grisim)
            print('')
            return 0
        else:
            err_msg = 'AXEPREP: No second normalization of image: %s!' % self.grisim
            raise aXeError(err_msg)

    def _check_second_gaincorr(self):
        """
        Check whether the gain correction had already been applied
        """
        from pyraf import iraf
        from pyraf.irafpar import IrafPar,IrafParI,IrafParS, IrafParList, makeIrafPar

        print('')
        print('AXEPREP: Image: %s has just been gain corrected!' % self.grisim)

        idec = IrafParS(['         Correct it again?[(y)es/(n)o/(q)uit] :(q)','string','h'],'whatever')
        idec.getWithPrompt()
        dec = idec.value.strip()
        if dec.upper() == 'Y':
            print('         Continue!')
            print('')
            return 1
        elif dec.upper() == 'N':
            print('         Not correcting image %s!' % self.grisim)
            print('')
            return 0
        else:
            err_msg = 'AXEPREP: No second normalization of image %s!' % self.grisim
            raise aXeError(err_msg)

    def _check_gain_correction(self):
        """
        Check whether the gain correction had already been applied
        """
        from pyraf import iraf
        from pyraf.irafpar import IrafPar,IrafParI,IrafParS, IrafParList, makeIrafPar

        print('')
        print('AXEPREP: Non-NICMOS images such as: %s usually are already gain corrected!' % self.grisim)

        idec = IrafParS(['         Correct it nevertheless?[(y)es/(n)o/(q)uit] :(q)','string','h'],'whatever')
        idec.getWithPrompt()
        dec = idec.value.strip()
        if dec.upper() == 'Y':
            print('         Continue!')
            print('')
            return 1
        elif dec.upper() == 'N':
            print('         Not correcting image %s!' % self.grisim)
            print('')
            return 0
        else:
            err_msg = 'AXEPREP: No gain correction for non-NICMOS images such as: %s!' % self.grisim
            raise aXeError(err_msg)

    def _transform_to_cps(self, ext_info, conf):
        """
        Transform the image from [e] to [e/s]
        """
        from pyraf import iraf

        from astropy.io import fits as pyfits

        dec = 1

        # make the names used in the IRAF copy commands
        # ext_image = self.grisim + '[SCI,'+str(ext_info['ext_version'])+']'
        # ext_image2 = self.grisim + '[SCI,'+str(ext_info['ext_version'])+',overwrite]'
        # err_image = self.grisim + '[ERR,'+str(ext_info['ext_version'])+']'
        # err_image2 = self.grisim + '[ERR,'+str(ext_info['ext_version'])+',overwrite]'


        # check for a previous normalization
        fits_img  = pyfits.open(axeutils.getIMAGE(self.grisim), 'readonly')
        fits_head = fits_img[ext_info['fits_ext']].header

        if 'AXEPRNOR' in fits_head:
            # check whether a second normalization
            # is really desired
            dec = self._check_second_normalization()

        # check for the exposure time keyword
        if not conf.get_gkey('EXPTIME'):
            exptime_kword = 'EXPTIME'
        else:
            exptime_kword = conf.get_gvalue('EXPTIME')

        # get the exposure time
        exptime = fits_img[0].header[exptime_kword]

        if dec:
            pstring = 'AXEPREP: Image %25s[SCI,%s] exposure time %7.1f.' % (self.grisim, str(ext_info['ext_version']), exptime)
            print(pstring)

            # Divide the grism image SCI and ERR arrays by exptime
            file_a = pyfits.open(axeutils.getIMAGE(self.grisim), 'update')
            file_a['SCI',ext_info['ext_version']].data /= exptime
            file_a['ERR',ext_info['ext_version']].data /= exptime
            file_a.close()

            # open the grism image and isolate the correct header
            grism_img    = pyfits.open(axeutils.getIMAGE(self.grisim), 'update')
            grism_header = grism_img[ext_info['fits_ext']].header

            # write a header entry
            grism_header['AXEPRNOR'] = ('Done',  'flag that exposure time normal. was done')

            # extract the value of the constant
            # sky subtracted in multidrizzle
            if 'MDRIZSKY' in fits_head:
                mdrizsky = fits_head['MDRIZSKY']
            else:
                mdrizsky = None

            # check whether a global bias subtraction was done
            # in axeprep
            #if backgr == 'YES':
            if 'backgr' in self.params and self.params['backgr']:

                # read the desctiptors written
                # in the global bias subtraction
                #hlist = ['SKY_SCAL','SKY_MAST']
                #kheader2 = get_header(putIMAGE(imagefile), ext='SCI', ver=ext,
                #                      list=hlist)
                sky_scal = fits_head['SKY_SCAL']
                sky_mast = fits_head['SKY_MAST']

                # compute the total subtracted background level
                # in multidrizzle and axeprep (in cps)
                if mdrizsky != None:
                    # sky_cps = (kheader2['SKY_SCAL'] * kheader2['SKY_MAST'] + kheader3['MDRIZSKY']) / kheader1[exptime_kword]
                    sky_cps = (sky_scal * sky_mast + mdrizsky) / exptime
                else:
                    # sky_cps = (kheader2['SKY_SCAL'] * kheader2['SKY_MAST']) / kheader1[exptime_kword]
                    sky_cps = (sky_scal * sky_mast) / exptime
            else:

                # compute the total subtracted bias level (in cps)
                if mdrizsky != None:
                    # sky_cps = kheader3['MDRIZSKY'] / kheader1[exptime_kword]
                    sky_cps = mdrizsky / exptime
                else:
                    sky_cps = 0.0

            # write the subtracted background level
            # into a defined descriptor for later use
            grism_header['SKY_CPS'] = ( sky_cps,  'sky level in cps')

            # close the fits image
            grism_img.close()

        else:
            return -1

        fits_img.close()

        return 0

    def _apply_gain_correction(self, ext_info):
        """
        Apply the gain correction
        """
        from pyraf import iraf

        from astropy.io import fits as pyfits

        """
        # extract the gain from the
        # header, make a float number from it
        #hlist = ['ADCGAIN']
        header = get_header(putIMAGE(grism_image), ext=0, ver=0, list=hlist, error=1)
        gain = float(header['ADCGAIN'])

        hlist = ['AXEGAINC']
        check = get_header(putIMAGE(grism_image), ext='SCI', ver=1,
                           list=hlist, error=0)
        if check != None:
            dec = report_warning(100,image=putIMAGE(grism_image))
        """

        # open the fits image
        fits_img  = pyfits.open(axeutils.getIMAGE(self.grisim), 'readonly')

        # get the gain
        # for NICMOS:
        gain = float(fits_img[0].header['ADCGAIN'])
        # for ACS/HRC:
        #gain = float(fits_img[0].header['CCDGAIN'])

        # get the header of the target extension
        fits_head = fits_img[ext_info['fits_ext']].header
        if 'AXEGAINC' in fits_head:
            dec = self._check_second_gaincorr()

        # close the fits image again
        fits_img.close()

        # multiply both the sci and the error array by the gain
        file_a = pyfits.open(axeutils.getIMAGE(self.grisim), 'update')
        file_a["SCI"].data *= gain
        file_a["ERR"].data *= gain
        file_a.close()

        # open the fits image
        fits_img  = pyfits.open(axeutils.getIMAGE(self.grisim), 'update')

        # write the flag into the science extension
        fits_img[ext_info['fits_ext']].header['AXEGAINC'] = ( 'Done',        'flag that gain correction was done')

        # write the flag into the error extension, assuming it to be next to the science extension
        fits_img[ext_info['fits_ext']+1].header['AXEGAINC'] = ( 'Done',        'flag that gain correction was done')

        # close the image
        fits_img.close()


    def run(self):
        """
        Run AXEPREP on one slitless image
        """
        from . import configfile

        # load the configuration files;
        # get the extension info
        conf = configfile.ConfigFile(axeutils.getCONF(self.config))
        ext_info = axeutils.get_ext_info(axeutils.getIMAGE(self.grisim), conf)

        # make a background PET if necessary
        if 'backgr' in self.params and self.params['backgr']:
            self._subtract_background(ext_info)

        # make a background PET if necessary
        if 'norm' in self.params and self.params['norm']:
            self._transform_to_cps(ext_info, conf)

        # check wheter the gain correction is desired
        if 'gcorr' in self.params and self.params['gcorr']:

            # check whether we have NICMOS data
            if not self._is_nicmos_data():

                # check whether gain correction IS desired
                if self._check_gain_correction():

                    # make the gain correction
                    self._apply_gain_correction(ext_info)

            else:

                # make the gain correction
                self._apply_gain_correction(ext_info)

        del conf

        # return something
        return 1
