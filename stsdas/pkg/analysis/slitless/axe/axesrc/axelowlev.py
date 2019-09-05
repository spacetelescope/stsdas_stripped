"""
$Revision: 1.14 $ $Date: 2010/03/22 15:43:24 $
Author: Martin Kuemmel (mkuemmel@stecf.org)
Affiliation: Space Telescope - European Coordinating Facility
WWW: http://www.stecf.org/software/slitless_software/axesim/
"""
from __future__ import absolute_import, print_function

__author__ = "Martin Kuemmel <mkuemmel@eso.org>"
__date__ = "$Date: 2010/03/22 15:43:24 $"
__version__ = "$Revision: 1.14 $"
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
import os
import os.path
import sys
import subprocess

from .axeerror import *
from .axeutils import *

# define the good return
# value for the binaries
GOOD_RETURN_VALUE = 0

class TaskWrapper(object):
    """
    General class to execute C-tasks
    """
    def __init__(self, taskname, tshort):
        """
        Initializer for the class

        @param taskname: name of the C-executable
        @type taskname: string
        @param tshort: shor name for task
        @type tshort: string
        """
        self.taskname = taskname
        self.tshort   = tshort

        # initialize the command list
        self.command_list = []

        # save a name for stdout
        self.stdout = getOUTPUT(tshort+'.stdout')

        # save a name for stderr
        self.stderr = getOUTPUT(tshort+'.stderr')

        # put the command into the list
        self.command_list.append(getBINDIR(taskname))

    def _cleanup(self):
        """
        Clean up some mess

        The method deletes the files created for stdout and stderr.
        This is a usual cleaning procedure in case nothing bad happened.
        """
        # delete stdout/stderr
        if os.path.isfile(self.stdout):
            os.unlink(self.stdout)
        if os.path.isfile(self.stderr):
            os.unlink(self.stderr)

    def _report_all(self, silent=True):
        """
        Print stdout and stderr on the screen

        The method gives a feedback in case of problems. stdout and stderr
        are both listed onto the screen for a further interactive analysis.

        @param silent: indicates silent/noisy runs
        @type silent: boolean
        """
        # check whether the command
        # was run silent
        if silent:
            # dump the files with
            # stdout and stderr onto the screen
            print('\nThere was a problem in the task: ', self.taskname)
            print('The output of the task (file "' + self.stdout + ')" is:')
            print('--------------------------------------------------------------------------------')
            for line in open(self.stdout):
                print(line.strip())
            print('\n\nThe error report is of the task (file "' + self.stdout +'") is:')
            print('--------------------------------------------------------------------------------')
            for line in open(self.stderr):
                print(line.strip())

        # report an error
        raise aXeError('An error occurred in the aXe task: ' + self.taskname)

    def run(self, silent=False):
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

        # is output desired
        if silent:
            # open stdout/stderr
            sout = open(self.stdout, 'w+')
            serr = open(self.stderr, 'w+')

            # execute the task
            retcode = subprocess.call(self.command_list, stdout=sout, stderr=serr)

            # close stdout/stderr
            sout.close()
            serr.close()

        else:

            # execute the task with the default stdout and
            # stderr, which is the system one
            print(self.command_list)
            retcode = subprocess.call(self.command_list)

        # return the result
        return retcode

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
        if retcode == GOOD_RETURN_VALUE:
            # do the cleaning
            self._cleanup()
        else:
            self._report_all(silent)

        # return the result
        return retcode


class aXe_AF2PET(TaskWrapper):
    """
    Wrapper around the aXe_AF2PET task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_AF2PET, self).__init__('aXe_AF2PET', 'af2pet')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # check for background flag
        if 'back' in params and params['back']:
            # put the bck-flag to the list
            self.command_list.append('-bck')

        # check for the in-AF name
        if 'in_af' in params and params['in_af'] != None:
            # put the name to the list
            self.command_list.append('-in_AF=%s' % params['in_af'])

        # check for the out-PET name
        if 'out_pet' in params and params['out_pet'] != None:
            # put the name to the list
            self.command_list.append('-out_PET=%s' % params['out_pet'])

class aXe_GPS(TaskWrapper):
    """
    Wrapper around the aXe_GPS task
    """
    def __init__(self, grism, config, beam_ref, xval, yval):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param beam_ref: beam to use
        @type beam_ref: string
        @param xval: x-position to check
        @type xval: float
        @param yval: y-position to check
        @type yval: float
        """
        # initialize via superclass
        super(aXe_GPS, self).__init__('aXe_GPS', 'axegps')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # put the beam name to the list
        self.command_list.append(beam_ref)

        # put the x-value to the list
        self.command_list.append(str(xval))

        # put the y-value to the list
        self.command_list.append(str(yval))


class aXe_BE(TaskWrapper):
    """
    Wrapper around the aXe_BE task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_BE, self).__init__('aXe_BE', 'backest')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # append the parameter 'np'
        if 'np' in params and params['np'] != None:
            self.command_list.append('-np=%s' % str(params['np']))

        # append the parameter 'interp'
        if 'interp' in params and params['interp'] != None:
            self.command_list.append('-interp=%s' % str(params['interp']))

        # append the parameter 'niter_med'
        if 'niter_med' in params and params['niter_med'] != None:
            self.command_list.append('-niter_med=%s' % str(params['niter_med']))

        # append the parameter 'niter_fit'
        if 'niter_fit' in params and params['niter_fit'] != None:
            self.command_list.append('-niter_fit=%s' % str(params['niter_fit']))

        # append the parameter 'kappa'
        if 'kappa' in params and params['kappa'] != None:
            self.command_list.append('-kappa=%s' % str(params['kappa']))

        # append the parameter 'smooth_length'
        if 'smooth_length' in params and params['smooth_length'] != None:
            self.command_list.append('-smooth_length=%s' % str(params['smooth_length']))

        # append the parameter 'smooth_length'
        if 'smooth_fwhm' in params and params['smooth_fwhm'] != None:
            self.command_list.append('-fwhm=%s' % str(params['smooth_fwhm']))

        # append the parameter 'af_file'
        if 'in_af' in params and params['in_af'] != None:
            self.command_list.append('-in_AF=%s' % params['in_af'])

        # append the parameter 'out_bck'
        if 'out_bck' in params and params['out_bck'] != None:
            self.command_list.append('-out_BCK=%s' % params['out_bck'])

        # append the flag 'old_bck'
        if 'old_bck' in params and params['old_bck']:
            self.command_list.append('-nor_flag')

        # append the flag 'mask'
        if 'mask' in params and params['mask']:
            self.command_list.append('-msk')

class aXe_DRZ2PET(TaskWrapper):
    """
    Wrapper around the aXe_DRZ2PET task
    """
    def __init__(self, inlist, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_DRZ2PET, self).__init__('aXe_DRZ2PET', 'drz2pet')

        # put the grism name to the list
        self.command_list.append(inlist)

        # put the config file name to the list
        self.command_list.append(config)

        # append the parameter 'out_pet'
        if 'out_pet' in params and params['out_pet'] != None:
            # put the name to the list
            self.command_list.append('-out_PET=%s' % params['out_pet'])

        # append the parameter 'in_af'
        if 'in_af' in params and params['in_af'] != None:
            # put the name to the list
            self.command_list.append('-in_AF=%s' % params['in_af'])

        # append the flag 'bck'
        if 'back' in params and params['back']:
            # put the bck-flag to the list
            self.command_list.append('-bck')

        # append the flag 'bck'
        if 'opt_extr' in params and params['opt_extr']:
            # put the bck-flag to the list
            self.command_list.append('-opt_extr')


class aXe_DRZPREP(TaskWrapper):
    """
    Wrapper around the aXe_DRZPREP task
    """
    def __init__(self, inlist, configs, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param inlist: name of the input image list
        @type inlist: string
        @param configs: aXe configuration file term
        @type configs: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_DRZPREP, self).__init__('aXe_DRZPREP', 'drzprep')

        # store the 'back'-flag
        if 'back' in params and params['back']:
            # put the opt_extr-flag to the list
            self.bck = True
        else:
            self.bck = False

        # put the grism name to the list
        self.command_list.append(inlist)

        # put the config file name to the list
        self.command_list.append(configs)

        # append the flag 'opt_extr'
        if 'opt_extr' in params and params['opt_extr']:
            # put the opt_extr-flag to the list
            self.command_list.append('-opt_extr')

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
        # run the method of the super class
        super(aXe_DRZPREP, self).runall(silent=silent)

        # check for the background flag
        if self.bck:
            # put the bck-flag to the list
            self.command_list.append('-bck')

            # run the method of the super class
            super(aXe_DRZPREP, self).runall(silent=silent)


class aXe_GOL2AF(TaskWrapper):
    """
    Wrapper around the aXe_GOL2AF task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_GOL2AF, self).__init__('aXe_GOL2AF', 'gol2af')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # append the parameter 'mfwhm'
        if 'mfwhm' in params and params['mfwhm'] != None:
            # put the name to the list
            self.command_list.append('-mfwhm=%s' % str(params['mfwhm']))

        # append the parameter 'dmag'
        if 'dmag' in params and params['dmag'] != None:
            # put the name to the list
            self.command_list.append('-dmag=%s' % str(params['dmag']))

        # append the parameter 'lambda_mark'
        if 'lambda_mark' in params and params['lambda_mark'] != None:
            # put the name to the list
            self.command_list.append('-lambda_mark=%s' % str(params['lambda_mark']))
        else:
            self.command_list.append('-lambda_mark=800.0')

        # append the parameter 'out_pet'
        if 'out_pet' in params and params['out_pet'] != None:
            # put the name to the list
            self.command_list.append('-out_PET=%s' % params['out_pet'])

        # append the parameter 'in_af'
        if 'out_af' in params and params['out_af'] != None:
            # put the name to the list
            self.command_list.append('-out_AF=%s' % params['out_af'])

        # append the parameter 'in_gol'
        if 'in_gol' in params and params['in_gol'] != None:
            # put the name to the list
            self.command_list.append('-in_GOL=%s' % params['in_gol'])

        # append the flag 'slitless_geom'
        if 'slitless_geom' in params and params['slitless_geom']:
            # put the exclude_faint-flag to the list
            self.command_list.append('-slitless_geom=1')
        else:
            self.command_list.append('-slitless_geom=0')

        # append the flag 'orient'
        if 'orient' in params and params['orient']:
            # put the exclude_faint-flag to the list
            self.command_list.append('-orient=1')
        else:
            self.command_list.append('-orient=0')

        # append the flag 'exclude'
        if 'exclude' in params and params['exclude']:
            # put the exclude_faint-flag to the list
            self.command_list.append('-exclude_faint')

       # append the flag 'bck'
        if 'back' in params and params['back']:
            # put the bck-flag to the list
            self.command_list.append('-bck')


class aXe_INTPIXCORR(TaskWrapper):
    """
    Wrapper around the aXe_INTPIXCORR task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_INTPIXCORR, self).__init__('aXe_INTPIXCORR', 'ipixcorr')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # check for the 'in_OAF' name
        if 'in_OAF' in params and params['in_OAF'] != None:
            # put the name to the list
            self.command_list.append('-in_OAF=%s' % str(params['in_oaf']))

        # check for the 'in_SPC' name
        if 'in_SPC' in params and params['in_SPC'] != None:
            # put the name to the list
            self.command_list.append('-in_SPC=%s' % str(params['in_spc']))

        # check for the 'out_SPC' name
        if 'out_SPC' in params and params['out_SPC'] != None:
            # put the name to the list
            self.command_list.append('-out_SPC=%s' % params['out_spc'])

        # check for the 'max_ext' value
        if 'max_ext' in params and params['max_ext'] != None:
            # put the value to the list
            self.command_list.append('-max_ext=%s' % str(params['max_ext']))
        else:
            # put the default to the list
            self.command_list.append('-max_ext=0.0')

       # append the flag 'ip_corr'
        if 'ip_corr' in params and params['ip_corr']:
            # put the ip_corr-flag to the list
            self.command_list.append('-ipixcorr')

       # append the flag 'nl_corr'
        if 'nl_corr' in params and params['nl_corr']:
            # put the nl_corr-flag to the list
            self.command_list.append('-nlincorr')

class aXe_NICBACK(TaskWrapper):
    """
    Wrapper
    """
    def __init__(self, grism, config, master_bck, back_ped=None):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_NICBACK, self).__init__('aXe_NICBACK', 'nicback')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # put the master background
        self.command_list.append(master_bck)

        if back_ped != None and len(back_ped) > 1:
            # put the master background
            self.command_list.append(back_ped)

class aXe_SCALEBCK(TaskWrapper):
    """
    Wrapper around the aXe_PET2SPC task
    """
    def __init__(self, grism, mask, config, master_sky, to_master=False,
                 make_plis=False):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param mask: name of the mask image
        @type mask: string
        @param config: name of the aXe configuration file
        @type config: string
        @param master_sky: name of the master sky image
        @type master_sky: string
        @param to_master: scale grism to master
        @type to_master: boolean
        @param make_plis: generate the pixel list
        @type make_plis: boolean
        """
        # initialize via superclass
        super(aXe_SCALEBCK, self).__init__('aXe_SCALEBCK', 'scalebck')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the mask name to the list
        self.command_list.append(mask)

        # put the config file name to the list
        self.command_list.append(config)

        # put the master background
        self.command_list.append(master_sky)

        if to_master:
            # add the flag for scaling to the master image
            self.command_list.append('-toMaster')

        if make_plis:
            # add flag to generate pixel list
            self.command_list.append('-make_plis')


class aXe_PET2SPC(TaskWrapper):
    """
    Wrapper around the aXe_PET2SPC task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_PET2SPC, self).__init__('aXe_PET2SPC', 'pet2spc')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # check for the 'in_AF' name
        if 'in_af' in params and params['in_af'] != None:
            # put the name to the list
            self.command_list.append('-in_AF=%s' % params['in_af'])

        # check for the 'OPET' name
        if 'opet' in params and params['opet'] != None:
            # put the name to the list
            self.command_list.append('-OPET=%s' % params['opet'])

        # check for the 'BPET' name
        if 'bpet' in params and params['bpet'] != None:
            # put the name to the list
            self.command_list.append('-BPET=%s' % params['bpet'])

        # check for the 'out_SPC' name
        if 'out_spc' in params and params['out_spc'] != None:
            # put the name to the list
            self.command_list.append('-out_SPC=%s' % params['out_spc'])

        # append the flag 'drzpath'
        if 'drzpath' in params and params['drzpath']:
            # put the ip_corr-flag to the list
            self.command_list.append('-drz')

        # append the flag 'noBPET'
        if 'use_bpet' in params and not params['use_bpet']:
            # put the ip_corr-flag to the list
            self.command_list.append('-noBPET')

        # append the flag 'opt_weights'
        if 'weights' in params and params['weights']:
            # put the ip_corr-flag to the list
            self.command_list.append('-opt_weights')

        # append the flag 'noflux'
        if 'do_flux' in params and not params['do_flux']:
            # put the ip_corr-flag to the list
            self.command_list.append('-noflux')

        # append the flag 'smooth_conv'
        if 'adj_sens' in params and params['adj_sens']:
            # put the ip_corr-flag to the list
            self.command_list.append('-smooth_conv')

class aXe_PETCONT(TaskWrapper):
    """
    Wrapper around the aXe_PETCONT task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_PETCONT, self).__init__('aXe_PETCONT', 'petcont')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # check for the 'in_AF' name
        if 'in_af' in params and params['in_af'] != None:
            # put the name to the list
            self.command_list.append('-in_AF=%s' % params['in_af'])

        # check for the 'specmodels' name
        if 'spec_models' in params and params['spec_models'] != None:
            # put the name to the list
            self.command_list.append('-model_spectra=%s' % params['spec_models'])

        # check for the 'objectmodels' name
        if 'object_models' in params and params['object_models'] != None:
            # put the name to the list
            self.command_list.append('-model_images=%s' % params['object_models'])

        # append the flag 'cont_model'
        if 'cont_model' in params and params['cont_model'] == 'gauss':
            # put the gauss-flag to the list
            self.command_list.append('-cont_model=1')
        elif 'cont_model' in params and params['cont_model'] == 'direct':
            # put the according number to the list
            self.command_list.append('-cont_model=2')
        elif 'cont_model' in params and params['cont_model'] == 'fluxcube':
            # put the according number to the list
            self.command_list.append('-cont_model=3')
        elif 'cont_model' in params and params['cont_model'] == 'geometric':
            # put the according number to the list
            self.command_list.append('-cont_model=4')

        # append the flag 'inter_type'
        if 'inter_type' in params and params['inter_type'] == 'linear':
            # put the according number to the list
            self.command_list.append('-inter_type=1')
        elif 'inter_type' in params and params['inter_type'] == 'polynomial':
            # put the according number to the list
            self.command_list.append('-inter_type=2')
        elif 'inter_type' in params and params['inter_type'] == 'spline':
            # put the according number to the list
            self.command_list.append('-inter_type=3')


        # append the parameter 'model_scale'
        if 'model_scale' in params and params['model_scale'] != None:
            # put the flag to the list
            self.command_list.append('-model_scale=%s' % str(params['model_scale']))

        # append the parameter 'lambda_psf'
        if 'lambda_psf' in params and params['lambda_psf'] != None:
            # put the number to the list
            self.command_list.append('-lambda_psf=%s' % str(params['lambda_psf']))
        else:
            self.command_list.append('-lambda_psf=800.0')

        # append the flag 'cont_map'
        if 'cont_map' in params and params['cont_map']:
            # put the cont_map-flag to the list
            self.command_list.append('-cont_map')

        # append the flag 'no_pet' to indicate
        # that no PET exists
        if 'no_pet' in params and params['no_pet']:
            # append the no-PET flagg
            self.command_list.append('-noPET')

class aXe_PETFF(TaskWrapper):
    """
    Wrapper around the aXe_PETFF task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_PETFF, self).__init__('aXe_PETFF', 'petff')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # check for the 'FFNAME' name
        if 'ffname' in params and params['ffname'] != None:
            # put the name to the list
            self.command_list.append('-FFNAME=%s' % params['ffname'])

        # append the flag 'bck'
        if 'back' in params and params['back']:
            # put the according flag to the list
            self.command_list.append('-bck')


class aXe_PETIPC(TaskWrapper):
    """
    Wrapper around the aXe_PETIPC task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_PETIPC, self).__init__('aXe_PETIPC', 'petipc')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # check for the 'in_OAF' name
        if 'in_OAF' in params and params['in_OAF'] != None:
            # put the name to the list
            self.command_list.append('-in_OAF=%s' % params['in_oaf'])

        # check for the 'in_PET' name
        if 'in_PET' in params and params['in_PET'] != None:
            # put the name to the list
            self.command_list.append('-in_PET=%s' % params['in_pet'])

        # check for the 'out_PET' name
        if 'out_PET' in params and params['out_PET'] != None:
            # put the name to the list
            self.command_list.append('-out_PET=%s' % params['out_pet'])

        # append the parameter 'max_ext'
        if 'max_ext' in params and params['max_ext'] != None:
            # put the number to the list
            self.command_list.append('-max_ext=%s' % str(params['max_ext']))

        # append the flag 'bck'
        if 'back' in params and params['back']:
            # put the according flag to the list
            self.command_list.append('-bck')

        # append the flag 'origname'
        if 'orig_name' in params and params['orig_name']:
            # put the according flag to the list
            self.command_list.append('-origname')


class aXe_SEX2GOL(TaskWrapper):
    """
    Wrapper around the aXe_SEX2GOL task
    """
    def __init__(self, grism, config, in_sex, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grismname: name of the dispersed image
        @type grismname: string
        @param configfile: name of the aXe configuration file
        @type configfile: string
        @param iolname: name of the input object list
        @type iolname: string
        @param dirname: name of the direct image
        @type dirname: string
        """
        # initialize via superclass
        super(aXe_SEX2GOL, self).__init__('aXe_SEX2GOL', 'sex2gol')

        # check whether a direct image exists
        if 'use_direct' in params and params['use_direct']:
            # put the direct image name to the list
            self.command_list.append(params['dirname'])

            # put the grism name to the list
            self.command_list.append(grism)

            # put the grism name to the list
            self.command_list.append(config)
        else:
            # put the grism name to the list
            self.command_list.append(grism)

            # put the grism name to the list
            self.command_list.append(config)

            # mark that there is no direct image
            self.command_list.append('-no_direct_image')

        # put the SExtractor cat to the list
        self.command_list.append('-in_SEX=%s' % in_sex)

        # check for the 'out_SEX' name
        if 'out_sex' in params and params['out_sex'] != None:
            # put the name to the list
            self.command_list.append('-out_SEX=%s' % params['out_sex'])

        # append the parameter 'dir_hdu'
        if 'dir_hdu' in params and params['dir_hdu'] != None:
            # put the number to the list
            self.command_list.append('-dir_hdu=%s' % str(params['dir_hdu']))

        # append the parameter 'spec_hdu'
        if 'spec_hdu' in params and params['spec_hdu'] != None:
            # put the number to the list
            self.command_list.append('-spec_hdu=%s' % str(params['spec_hdu']))


class aXe_STAMPS(TaskWrapper):
    """
    Wrapper around the aXe_STAMPS task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_STAMPS, self).__init__('aXe_STAMPS', 'stamps')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # append the parameter 'in_af'
        if 'in_af' in params and params['in_af'] != None:
            # put the name to the list
            self.command_list.append('-in_AF=%s' % params['in_af'])

        # check for the 'in_PET' name
        if 'in_pet' in params and params['in_pet'] != None:
            # put the name to the list
            self.command_list.append('-in_PET=%s' % params['in_pet'])

        # check for the 'out_STP' name
        if 'out_stp' in params and params['out_stp'] != None:
            # put the name to the list
            self.command_list.append('-out_STP=%s' % params['out_stp'])

        # append the flag 'drz'
        if 'sampling' in params and params['sampling'] == 'drizzle':
            # put the flagg to the list
            self.command_list.append('-drzstamp')
        elif 'sampling' in params and params['sampling'] == 'rectified':
            # put the flagg to the list
            self.command_list.append('-rectified')

        # append the flag 'drz'
        if 'drzpath' in params and params['drzpath']:
            # put the according flag to the list
            self.command_list.append('-drz')


class aXe_TRACEFIT(TaskWrapper):
    """
    Wrapper around the aXe_TRACEFIT task
    """
    def __init__(self, grism, config, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param grism: name of the dispersed image
        @type grism: string
        @param config: name of the aXe configuration file
        @type config: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_TRACEFIT, self).__init__('aXe_TRACEFIT', 'tracefit')

        # put the grism name to the list
        self.command_list.append(grism)

        # put the config file name to the list
        self.command_list.append(config)

        # append the parameter 'in_af'
        if 'in_af' in params and params['in_af'] != None:
            # put the name to the list
            self.command_list.append('-in_AF=%s' % params['in_af'])


class aXe_FILET(TaskWrapper):
    """
    Wrapper around the aXe_STAMPS task
    """
    def __init__(self, dppfile, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param dppfile: name of the dpp-file
        @type dppfile: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_FILET, self).__init__('aXe_FILET', 'filet')

        # put the grism name to the list
        self.command_list.append(dppfile)

        # append the 'opt_extr' flag
        if 'opt_extr' in params and params['opt_extr']:
            self.command_list.append('-opt_extr')

            # append the parameter 'in_af'
        if 'drztmp' in params and params['drztmp'] != None:
            # put the name to the list
            self.command_list.append('-drztmp=%s' % params['drztmp'])

class aXe_DIRIMAGE(TaskWrapper):
    """
    Wrapper around the aXe_DIRIMAGE task
    """
    def __init__(self, dirname, config, tpass_direct, **params):
        """
        Initializer for the class

        This method is a simple initializer for the class. All
        variables a transferred to a list, if necessary with the
        appropriate leading parameter name

        @param dirname: name of the direct image
        @type dirname: string
        @param configfile: name of the aXe configuration file
        @type configfile: string
        @param **params: all other parameters
        @type **params: dictionary
        """
        # initialize via superclass
        super(aXe_DIRIMAGE, self).__init__('aXe_DIRIMAGE', 'dirimage')

        # put the direct image name to the list
        self.command_list.append(dirname)

        # put the aXe configuration file name to the list
        self.command_list.append(config)

        # put the total passband file name to the list
        self.command_list.append(tpass_direct)

        # check whether model spectra are given
        # append the file name to the list
        if 'model_spectra' in params and params['model_spectra'] != None:
            #self.command_list.append('-model_spectra='+str(model_spectra))
            self.command_list.append('-model_spectra=%s' % params['model_spectra'])

        # check whether model images are given
        # append the file name to the list
        if 'model_images' in params and params['model_images'] != None:
            self.command_list.append('-model_images=%s' % params['model_images'])

        # check whether model images are given
        # append the file name to the list
        if 'tel_area' in params and params['tel_area'] != None:
            self.command_list.append('-tel_area=%f' % params['tel_area'])

        # append the model scale, give default if necessary
        if 'model_scale' in params and params['model_scale'] != None:
            self.command_list.append('-model_scale=%f' % params['model_scale'])
        else:
            self.command_list.append('-model_scale=5.0')


