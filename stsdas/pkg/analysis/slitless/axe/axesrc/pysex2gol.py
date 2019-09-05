"""
$Revision: 1.10 $ $Date: 2010/05/21 13:32:22 $
Author: Martin Kuemmel (mkuemmel@stecf.org)
Affiliation: Space Telescope - European Coordinating Facility
WWW: http://www.stecf.org/software/slitless_software/axesim/
"""
from __future__ import absolute_import, print_function

__author__ = "Martin Kuemmel <mkuemmel@eso.org>"
__date__ = "$Date: 2010/05/21 13:32:22 $"
__version__ = "$Revision: 1.10 $"
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
from axe import axe_asciidata
from .axeerror import aXeError

class Sex2GolPy(object):
    """
    Class for the SEX2GOL task
    """
    def __init__(self, grisim, config, in_sex=None, dirname=None, out_sex=None, spec_hdu=None, dir_hdu=None):
        """
        Initializes the class
        """
        from . import axeutils
        
        # store some parameters
        self.grisim  = grisim
        self.config  = config
        self.in_sex  = in_sex
        self.dirname =  dirname

        # determine the grism image extensions
        self.grism_extinfo = self._get_grism_ext_info(grisim, config, spec_hdu)

        # determine the grism image extensions
        self.dirname, self.dirname_extinfo = self._get_dirname_information(dirname, config, grisim, self.grism_extinfo, dir_hdu)

        # get information on the input and output lists
        self.in_sex, self.out_sex = self._resolve_list_names(self.dirname, self.dirname_extinfo, self.grisim, self.grism_extinfo, in_sex, out_sex)

        # save a name for stdout
        self.stdout = axeutils.getOUTPUT('pysex2gol.stdout')

    def __str__(self):
        """
        String method for the class
        """
        # define the prefix
        prefix = 'py_SEX2GOL: '

        # compose the feedback
        big_str  = '%s Setup:\n' % prefix
        big_str += '%s Input g/prism image:      %s\n' % (prefix, self.grisim)
        big_str += '%s Configuration file name:  %s\n' % (prefix, self.config)
        big_str += '%s Direct image:             %s\n' % (prefix, self.dirname)
        big_str += '%s G/Prism extension:        %s\n' % (prefix, self.grism_extinfo['axe_ext'])
        big_str += '%s Direct image extension:   %s\n' % (prefix, self.dirname_extinfo['axe_ext'])
        big_str += '%s Input catalog name:       %s\n' % (prefix, self.in_sex)
        big_str += '%s Output catalog name:      %s  ' % (prefix, self.out_sex)

        # return the string
        return big_str

    def _cleanup(self):
        """
        Clean up some mess

        The method deletes the files created for stdout.
        This is a usual cleaning procedure in case nothing bad happened.
        """
        import os
        import os.path
        # delete stdout/stderr
        if os.path.isfile(self.stdout):
            os.unlink(self.stdout)

    def _get_grism_ext_info(self, grisim, config, spec_hdu=None):
        """
        Determine the extension information on the grism image
        """
        from . import axeutils
        from . import configfile

        # check for an explicit extension
        if spec_hdu == None:
            # load the configuration file;
            # determine the extension information
            conf = configfile.ConfigFile(axeutils.getCONF(config))
            ext_info = axeutils.get_ext_info(axeutils.getIMAGE(grisim), conf)
            del conf

        else:
            # make by hand the extension information
            ext_info = {'axe_ext': spec_hdu, 'fits_ext': spec_hdu-1}

        # return the extension info
        return ext_info

    def _get_dirname_information(self, dirname, config, grisim, grism_extinfo, dir_hdu=None):
        """
        Determine the direct image information
        """
        from . import axeutils
        from . import configfile

        # check whether ANY direct image information exists
        if dirname == None and dir_hdu == None:
            # set the grism image as direct image
            dirname         = grisim
            dirname_extinfo = grism_extinfo

        elif dirname != None and dir_hdu == None:
            # load the configuration file;
            # determine the extension information
            conf = configfile.ConfigFile(axeutils.getCONF(config))
            dirname_extinfo = axeutils.get_ext_info(axeutils.getIMAGE(grisim), conf)
            del conf

        elif dirname != None and dir_hdu != None:
            # make by hand the extension information
            dirname_extinfo = {'axe_ext': dir_hdu, 'fits_ext': dir_hdu-1}

        else:
            # error and out
            err_msg = 'Specifying NO direct image but a direct image HDU: %i makrs NO sense!' % dir_hdu
            raise aXeError(err_msg)

        # return the name and the extension info
        return dirname, dirname_extinfo

    def _resolve_list_names(self, dirname, dirname_extinfo, grisim, grism_extinfo, in_sex, out_sex):
        """
        Determine the lists for input and output
        """
        import os
        import os.path

        from . import axeutils

        # compose the default name
        if in_sex == None:
            # compose the filename from the direct image name
            in_sex = dirname.replace('.fits', '_%i.cat' % dirname_extinfo['axe_ext'])

        # check whether the explicitly given filename exists
        if not os.path.isfile(in_sex):
            err_msg = 'The Input Object List: %s does not exist!' % in_sex
            raise aXeError(err_msg)

        if out_sex == None:
            # compose the name for the output GOL
            out_sex = os.path.basename(grisim).replace('.fits', '_%i.cat' % grism_extinfo['axe_ext'])

        # return the IOL and the GOL names
        return in_sex, out_sex

    def _transfer_fromIOL_toGOL(self):
        """
        Copies relevant data from IOL to GOL
        """
        from . import axeiol

        # load the IOL
        iol = axeiol.InputObjectList(self.in_sex)

        # check for an empty table
        if iol.nrows < 1:

            # return only none's
            return None, None 

        # determine the number of columns
        ncols_gol = len(iol.mand_cols) + len(iol.wav_cols)

        # create an empty GOL
        gol = axe_asciidata.create(ncols_gol, iol.nrows)
        gol.toSExtractor()

        # go over all
        # mandatory columns
        c_index = 0
        for one_col in iol.mand_cols:
            # rename the GOL column
            gol[c_index].rename(one_col['name'])

            # go over all rows
            for r_index in range(iol.nrows):

                # transfer the value
                gol[one_col['name']][r_index] = iol[one_col['name']][r_index]

            # enhance the index
            c_index += 1

        # go over all wavelength columns
        for one_col in iol.wav_cols:

            # rename the GOL column
            gol[c_index].rename(one_col['name'])

            # go over all rows
            for r_index in range(iol.nrows):

                # transfer the value
                gol[one_col['name']][r_index] = iol[one_col['name']][r_index]

            # enhance the index
            c_index += 1

        # return the GOL
        return iol, gol

    def _transfer_coos(self, iol, gol):
        """
        Transfer coordinates from the IOL to the GOL
        """
        from . import axeutils

        import stsci.tools as pytools
        from stsci.tools import wcsutil

        # compose the WCS-term for the direct and grism images
        dir_term = axeutils.getIMAGE('%s[%i]' % (self.dirname,self.dirname_extinfo['fits_ext']))
        gri_term = axeutils.getIMAGE('%s[%i]' % (self.grisim, self.grism_extinfo['fits_ext']))

        # generate the WCS objects
        dir_wcs = wcsutil.WCSObject(dir_term)
        gri_wcs = wcsutil.WCSObject(gri_term)

        # go over each row
        for index in range(iol.nrows):

            # make a position tuple
            xy_dirname = (iol['X_IMAGE'][index], iol['Y_IMAGE'][index])

            # convert to RADEC
            radec_pos = dir_wcs.xy2rd(xy_dirname)

            # convert to XY on grism
            xy_grism  = gri_wcs.rd2xy(radec_pos)

            # store projected vals in the GOL
            gol['X_IMAGE'][index] = float(xy_grism[0])
            gol['Y_IMAGE'][index] = float(xy_grism[1])

    def _treat_NULL_table(self, out_name):
        """
        Transfer an empty table

        The header of the empty table is written to the
        GOL file. However then no check is done whether 
        the column is complete.
        """
        # open the "GOL"
        out_file = file(out_name, 'w+')
        
        # go over the "IOL"
        for a_line in file(self.in_sex):
            # transfer everyhing
            out_file.write(a_line)

        # close the file
        out_file.close()

    def run(self, silent=False):
        """
        Make the SEX2GOL transformations
        """
        from . import axeutils

        # give feedback
        if not silent:
            print(self)
            print('py_SEX2GOL:  Start processing ...',)
        else:
            # open stdout/stderr
            sout = open(self.stdout, 'w+')
            sout.write(str(self)+'\n')
            sout.write('py_SEX2GOL:  Start processing ...')

        # copy the relevant data to the GOL
        iol, gol = self._transfer_fromIOL_toGOL()

        # check whether something can be done
        if iol !=None and gol != None:

            # transfer the coordinates
            self._transfer_coos(iol, gol)

            # store the GOL
            gol.writeto(axeutils.getOUTPUT(self.out_sex))

        else:
            # if there are no objects, just copy the empty table 
            # header to the GOL
            self._treat_NULL_table(axeutils.getOUTPUT(self.out_sex))

            # give feedback
            if not silent:
                print(self)
                print('py_SEX2GOL:  Warning! Empty table copied to GOL')
            else:
                # open stdout/stderr
                sout.write('py_SEX2GOL:  Warning! Empty table copied to GOL')
        
        # give feedback
        if not silent:
            print('     Done')
        else:
            sout.write('     Done\n')
            sout.close()

    def runall(self, silent=False):
        """
        Run the wrapped task

        The method executes the associated C-executable. The return code given
        by the C-executable is returned. In silent mode stdout and stderr
        are writtren to a file, in non-silent mode to the screen.

        @param silent: boolean for silent mode
        @type silent: boolean

        @return: the return code of the C-executable
        @rtype: int
        """
        # run the executable
        retcode = self.run(silent=silent)

        # check whether the run was good
        if silent:
            # do the cleaning
            self._cleanup()
