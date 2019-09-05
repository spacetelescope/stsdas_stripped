"""
$Revision: 1.6 $ $Date: 2010/01/18 09:16:15 $
Author: Martin Kuemmel (mkuemmel@stecf.org)
Affiliation: Space Telescope - European Coordinating Facility
WWW: http://www.stecf.org/software/slitless_software/axesim/
"""
from __future__ import absolute_import, print_function

__author__ = "Martin Kuemmel <mkuemmel@eso.org>"
__date__ = "$Date: 2010/01/18 09:16:15 $"
__version__ = "$Revision: 1.6 $"
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
import math

from . import axeutils
from .axeerror import aXeError

class ConfigList(object):
    """
    Configuration File Object
    """
    def __init__(self, keylist, header=None):
        """
        Initializer for the ConfigFile object

        Initializes the ConfigList object by tranfsforming
        a list of keywords into a structured list including
        beams descriptions

        @param keylist: list of configuration keys
        @type keylist: [ConfKey]
        @param header: the header string
        @type header: [string]
        """
        # beam inices which might be found the file
        idents = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q']

        # create the (visible) dictionary
        self.beams  = {}

        # create the hidden beam list
        self._beams = []

        # store the header
        self.header = header

        # load the genral keywords
        self.gkeys  = self._find_gkeys(keylist)


        # try to load beams as long as there
        # are keywords and as long as there
        # are candidate beam numbers
        iindex=0
        while (len(keylist) > 0 and iindex < len(idents)):
            try:
                # try to load a beam
                self._beams.append(ConfigBeam(idents[iindex], keylist))
                self.beams[idents[iindex]] = self._beams[iindex]

            except BeamNotFound:
                # no information on this beam is in the file
                pass

            # enhance the counter
            iindex += 1

        # inform about the useless keywords
        if len(keylist) > 0:
            print('\nDispensable Keywords: ')
            for key in keylist:
                print(str(key), end=' ')

    def __str__(self):
        """
        String method for the class

        The method transoforms the configuration
        file object into its string representation.

        @return: string representation of the object
        @rtype: String
        """
        # take the string of the header
        rstring = str(self.header) + '\n'

        # add the strings for the global keys
        for key in self.gkeys:
            rstring += str(key)

        for beam in self._beams:
            rstring += str(beam)

        # return the total string
        return rstring

    def __delitem__(self, item):

        # find the index of the requested item
        index = self._find_gkey(item)

        # check whether the item was found
        if index  > -1:
            # return the identified item
            del self.gkeys[index]

    def __getitem__(self, item):

        # find the index of the requested item
        index = self._find_gkey(item)

        # check whether the item was found
        if index  > -1:
            # return the identified item
            return self.gkeys[index].keyvalue
        else:
            if item in self.beams.keys():
                return self.beams[item]
            else:
                # return NULL
                return None

    def _find_gkey(self, item):

        # set the default return value
        found = -1

        # go over all items
        for index in range(len(self.gkeys)):
            # check whether it is the right item
            if self.gkeys[index].keyword == item:
                # set the return value to the index
                found = index

        # return the result
        return found

    def _load_file(self, filename):
        """
        Configuration file --> keyword list

        The method load a configuration file and
        extract all valid keyword-keyvalue-comment information
        from it. The keyword-keyvalue pairs are
        organized and returned as a list of
        configuration key objects.

        @param filename: name of the configuration file
        @type filename: String

        @return: list of ConfKey's
        @rtype: [ConfKey]
        """
        # initialize the liust
        keylist = []

        # open the file and parse through it
        fopen = open(filename, 'r')
        for line in fopen:
            # strip the line
            str_line = line.strip()

            # check whether the line contains a keyword
            if len(str_line) and str_line[0] != '#':
                # create and append the keyword
                keylist.append(self._key_from_line(str_line))

        # close the file
        fopen.close()

        # return the list
        return keylist

    def _get_gkey_index(self, keyword):
        """
        Retrieve the index of a global keyword

        The method searches for the index of
        a requested keyword in the list of global
        keywords. If the keyword does not exists,
        the index -1 is returned

        @param keyword: name of the requested keyword
        @type keyword: String

        @return: the index of the keyword
        @rtype: integer
        """
        # initialize the return value
        kindex = -1

        # go over all keys
        for index in range(len(self.gkeys)):
            # check whether the current key matches
            if self.gkeys[index].keyword == keyword:
                # return it if it matches
                return index

        # return the default
        return kindex

    def _key_from_line(self, line):
        """
        Creates a keyword from a line

        The method extracts the konfiguration keyword,
        the associated value and, if present,
        a comment from a line in the configuration file.
        A configuration key object representing the extracted
        keyword is created and returned.

        @param line: line to analyze
        @type line: String

        @return: configuration key  object
        @rtype: ConfKey
        """
        # split the line into items
        items = line.split()

        # for more than one item the
        # first item is the keyword
        if len(items) > 1:
            keyword  = items[0].strip()

            # check for a comment
            cpos = line.rfind(';')
            if cpos < 0:
                # evaluate the keyvalue
                keyvalue = line[line.find(keyword)+len(keyword):].strip()
                comment  = None
            else:
                # evalute keyvalue and comment
                tmp_val  = line[line.find(keyword)+len(keyword):].strip()
                keyvalue = tmp_val.split(';')[0].strip()
                comment  = tmp_val.split(';')[1].strip()
        else:
            # something's wrong here
            err_msg = 'Only one item in: ' + line + ' !'
            raise aXeError(err_msg)

        # create and return the keyword
        return ConfKey(keyword, keyvalue, comment)

    def _find_gkeys(self, keylist):
        """
        Finds and extracts the global keywords

        The method finds the all predefined global keywords in
        a keyword list. The list of globale keywords is
        returned. They counterparts in the input keyword list
        are deleted.

        @param keylist: list of keywords
        @type keylist: [ConfKey]

        @return: global keywords
        @rtype: [ConfKey]
        """
        gkeywords = ['INSTRUMENT', 'CAMERA', 'TELAREA',
                     'SCIENCE_EXT', 'ERRORS_EXT',
                     'DQ_EXT', 'OPTKEY1', 'OPTVAL1', 'FFNAME', 'DQMASK',
                     'DRZRESOLA', 'DRZSCALE', 'DRZLAMB0', 'DRZXINI',
                     'DRZROOT', 'EXPTIME', 'WEIGHT_EXT', 'DRZPFRAC',
                     'DRZPSCALE', 'DRZKERNEL', 'MODEL_EXT', 'VARIANCE_EXT',
                     'RDNOISE', 'PSFCOEFFS', 'PSFRANGE', 'IPIXFUNCTION',
                     'POBJSIZE', 'SMFACTOR']

        # initialize the global keylist
        # and the list with indices to be deleted
        gkeys  = []
        dindex = []

        # go over the keylist read in,
        # keeping and index variable
        iindex=0
        for key in keylist:

            # identify the current keyword in the
            # list of possible ones
            if key.keyword in gkeywords:

                # store the index
                dindex.append(iindex)

                # create and append the new keyword
                gkeys.append(ConfKey(key.keyword, key.keyvalue, key.comment))

            iindex += 1

        # delete the input keywords which
        # have been 'used'
        dindex.sort()
        dindex.reverse()
        for index in dindex:
            del keylist[index]

        # return the list of global keys
        return gkeys

    def _check_gfiles(self):
        """
        Checks whether all files exist

        The method checks whether the files whose names
        are within the class data do exist or not.
        An error is reported in case that the files
        do not exist.
        """
        # list of the root of all
        # global keys indicating a file
        fkeys = ['FFNAME']

        # go over all file keywords
        for key in fkeys:

            # identify the keyword in the list
            index = self._get_gkey_index(key)

            # check for existence
            if index > -1:

                # extract the keyvalue
                kvalue = self.gkeys[index].keyvalue

                # if the keyvalue is NOT None but the file does not exist
                if kvalue.upper() != 'NONE' and not os.path.isfile(axeutils.getCONF(kvalue)):
                    # report an error
                    err_msg = 'The file: %s does not exist!' % axeutils.getCONF(kvalue)
                    raise aXeError(err_msg)

    def get_gkey(self, keyword):
        """
        Retrieve a requested global keyword

        The method searches the list of global keywords
        for a fitting keyword. In case that the requested
        keyword exists, it is returned.
        If not 'None' is returned

        @param keyword: name of the requested keyword
        @type keyword: String

        @return: the requested keyword or 'None'
        @rtype: ConfKey
        """
        # initialize the return value
        rkey = None

        # search for the index in the keyword list
        index = self._get_gkey_index(keyword)

        # check whether the keyword exists
        if index > -1:
            # return the keyword
            return self.gkeys[index]
        else:
            # return the default
            return rkey

    def add_gkey(self, keyword, keyvalue, comment=None):
        """
        Add global keyword

        The method adds a keyword to the list of global
        keywords. In case that the keyword just exists,
        it is overwritten, otherwise it is appended
        to the global keyword list.

        @param keyword: name of the requested keyword
        @type keyword: String
        @param keyvalue: value of the requested keyword
        @type keyvalue: any type
        @param comment: comment for the keyword
        @type comment: String
        """

        # search for the index in the keyword list
        index = self._get_gkey_index(keyword)

        if index > -1:
            # if it matches, copy the data
            self.gkeys[index].keyvalue = keyvalue
            self.gkeys[index].comment  = comment
        else:
            # the keyword does not yet exist, just create and add it
            self.gkeys.append(ConfKey(keyword, keyvalue, comment))

    def drizzle_check(self):
        """
        Check for drizzle keywords

        The method assures that all necessary drizzle keywords
        are present. Nonexisting keywords are added with default
        values. Finally the value for the drizzle kernel is checked
        against all valid values.

        @return: information whether the drizzle kernel is valid
        @rtype: integer
        """
        # list with all valid kernels
        kernels = ['square','point','turbo','gaussian','tophat','lanczos2','lanczos3']

        # make sure that some important drizzle keywords are there
        pscale = self.setdefault('DRZPSCALE',1.0)
        pfrac  = self.setdefault('DRZPFRAC',1.0)
        dkernel= self.setdefault('DRZKERNEL','square')
        droot  = self.setdefault('DRZROOT','aXedrizzle')

        # check for valid drizzle kernel
        if dkernel not in kernels:
            # return a warning
            return 1

        # return the default
        return 0


    def setdefault(self, keyword, keyvalue, comment=None):
        """
        Add global keyword

        The method mimics the setdefault method for dictionary
        objects. A keyword is added with the given value and
        comment, but only in case that it does not yet exist.
        If it exists, nothing is done

        @param keyword: name of the requested keyword
        @type keyword: String
        @param keyvalue: value of the requested keyword
        @type keyvalue: any type
        @param comment: comment for the keyword
        @type comment: String

        @return: the requested keyvalue
        @rtype: any
        """

        # search for the index in the keyword list
        index = self._get_gkey_index(keyword)

        if index < 0:
            # the keyword does not yet exist, just create and add it
            self.gkeys.append(ConfKey(keyword, keyvalue, comment))
            # extract the keyvalue
            value = self.gkeys[-1].keyvalue
        else:
            # extract the keyvalue
            value =  self.gkeys[index].keyvalue

        # return the keyvalue
        return value

    def get_gvalue(self, keyword):
        """
        Retrieve a requested global keyword value

        The method returns the value of the keyword
        which matches the requested value.
        If there is no matching keyword, 'None'
        is returned.

        @param keyword: name of the requested keyword
        @type keyword: String

        @return: the requested keyvalue or 'None'
        @rtype: String
        """

        # set the default return value
        rvalue = None

        # search for the keyword
        key = self.get_gkey(keyword)

        # check whether it is non-NULL
        if key:
            # extract the value
            rvalue = key.keyvalue

        # return the value
        return rvalue

    def writeto(self, filename):
        """
        Save the object to a file

        The method saves the object to a file
        with name specified in the input.

        @param filename: name of the file
        @type filename: String
        """
        # destroy the old file
        if os.path.isfile(filename):
            os.unlink(filename)

        # open the new file
        ofile = open(filename, 'w')

        # write the string to the file
        ofile.write(str(self))

        # close the file
        ofile.close()


    def flush(self):
        """
        Save the object back to file

        The method saves the object back to a file
        with the identical filename it was read from.
        """
        # just use the more general method
        self.writeto(self.filename)

    def check_files(self, check_glob=True):
        """
        Checks whether all files exist

        The method checks whether the files whose names
        are within the class data do exist or not.
        An error is reported in case that the files
        do not exist.
        """
        n_sens = 0

        # check global files if desired
        if check_glob:
            self._check_gfiles()

        # create the (visible) dictionary
        for bkey in self.beams.keys():
            n_sens += self.beams[bkey].check_files()

        # return the number
        # of existing sensitivity files
        return n_sens

class ConfigFile(ConfigList):
    """
    Configuration File Object
    """
    def __init__(self, filename=None):
        """
        Initializer for the ConfigFile object

        Initializes the ConfigFile object either
        by reading in a configuration file
        or by creating a default configuration file

        @param filename: name of the configuration file
        @type filename: String
        """

        # check if a filename is given
        if filename is None:
            # load the default
            print('No file given, can do nothing!!')
        else:
            # safe the file name
            self.filename = filename

            # create a keyword list
            keylist = self._load_file(filename)

            # load the header
            header = ConfHeader(filename)

            super(ConfigFile, self).__init__(keylist, header)

    def _get_simul_name(self):
        """
        Get the filename used in aXeSIM
        """
        # just add '.simul' and return the result
        return self.filename + '.simul'

    def confirm_extrkeys(self):
        """
        Confirm that all keywords for the extraction exist
        """
        # default is true!
        extr_ready = 1

        # check existence of 'POBJSIZE'
        if self['POBJSIZE'] == None:
            extr_ready = 0
        # check for reasonable value
        elif float(self['POBJSIZE']) < 0.0:
            extr_ready = 0

        # check existence of 'SMFACTOR'
        if self['SMFACTOR'] == None:
            extr_ready = 0
        # check for reasonable value
        elif float(self['SMFACTOR']) < 0.0:
            extr_ready = 0

        # return the value
        return extr_ready

    def confirm_lambda_psf(self):
        """
        Check whether a 'lambda_psf' value is needed, provide one
        """
        # check whether 'lambda_psf' is needed
        if self['PSFCOEFFS'] != None and self['PSFRANGE'] != None:
            # split the term
            psf_range = self['PSFRANGE'].split()

            # extract the defined range as float
            lambda_min = float(psf_range[0])
            lambda_max = float(psf_range[1])

            # make 'lambda_psf' to the mean value
            lambda_psf = 0.5 * (lambda_max + lambda_min)
        else:
            # leave it at None
            lambda_psf = None

        # return the value
        return lambda_psf

    def axesim_prep(self):
        """
        Removes modifies some keywords
        """
        # derive the new configuration file name
        new_name = self._get_simul_name()

        # check whether the science extension has other
        # than the allowed values
        if self['SCIENCE_EXT'] != 'SCI' and self['SCIENCE_EXT'] != '2':

            # find the index of the sceicne extension
            index = self._find_gkey('SCIENCE_EXT')

            # check whether the item was found
            if index  > -1:
                # set it to the allowed value
                self.gkeys[index].keyvalue = 'SCI'


        # check whether the telesocpe are is known
        if self['TELAREA'] == None:
            # set the telescope are to the
            # Hubble default
            self.add_gkey('TELAREA', 45238.93)

        index = 1
        while self['OPTKEY'+str(index)] != None:
            del self['OPTKEY'+str(index)]
            del self['OPTVAL'+str(index)]
            index += 1

        # just make sure that
        # the error=- and dq-
        # extensions are set
        self.add_gkey('ERRORS_EXT', 'ERR')
        self.add_gkey('DQ_EXT', 'DQ')

        # write the file back
        self.writeto(new_name)

        # return the baseic filename of the
        # simulation configuration file
        return os.path.basename(new_name)

class ConfigBeam(object):
    """
    Configuration Beam object
    """
    def __init__(self, ident=None, keylist=None):
        """
        Initializer for the ConfigBeam object

        A configuration beam object is intialized. This is done
        by either extracting the relevant keywords for a certain
        beam from a keyword list or creating a default beam.

        @param ident: beam identification
        @type ident: Character
        @param keylist: list of keywords
        @type keylist: [ConfKey]
        """

        # check if a filename is given
        if ident is None or keylist is None:
            # load the default
            print('No ID or no keywords given, can do nothing!!')
        else:
            # try to load the beam keywords
            try:
                # store the ident
                self.ident = ident

                # load the general beam keywords
                self.beamkeys  = self._find_beamkeys(ident, keylist)

                # load the trace keywords
                self.trace = ConfigTrace(ident, keylist)

                # load the dispersion keywords
                self.disp  = ConfigDisp(ident, keylist)

            # catch a pure CKeyNotFound exception
            # which is raised if a beam is competely
            # absent in the keyword list
            except CKeyNotFound:
                raise BeamNotFound(ident)

    def __str__(self):
        """
        String method for the class

        The method transforms theconfiguration
        beam object into its string representation.

        @return: string representation of the object
        @rtype: string
        """
        # initialize the return string
        rstring = '\n#-----------\n#\n# Beam '+ str(self.ident) + ':\n#\n#-----------\n'

        # add the strings for the global keys
        for key in self.beamkeys:
            rstring += str(key)

        # add the string for the trace
        rstring += str(self.trace)

        # add the string for the dispersion
        # solution
        rstring += str(self.disp)

        # return the total string
        return rstring

    def __getitem__(self, item):

        full_item = item + self.ident

        rvalue =  self.get_bvalue(full_item)

        return rvalue

    def __setitem__(self, item, value):

        full_item = item + self.ident

        index =  self._get_bkey_index(full_item)

        if index > -1:
            self.beamkeys[index].keyvalue = value

    def _find_beamkeys(self, ident, keylist):
        """
        Load the global beam keywords

        The method extracts all global beam keywords
        from a keyword list. The extracted keywords are returned
        as a list. They are removed from the input list.

        @param ident: beam identification
        @type ident: Character
        @param keylist: list of keywords
        @type keylist: [ConfKey]
        """

        # list of the root of all globale
        # beamword keys
        bkeys = ['BEAM', 'MMAG_EXTRACT_', 'MMAG_MARK_', 'XOFF_',
                 'YOFF_', 'SENSITIVITY_']

        # list of optional keywords
        okeys = ['PSF_OFFSET_']

        # appen the beam identifier to the
        # keyword roots to get a list of keywords
        # to search for
        id_keys = []
        for key in bkeys:
            id_keys.append(key + ident)

        # initiate and fill
        # collect a list of optional keywords
        opt_keys = []
        for key in okeys:
            opt_keys.append(key + ident)

        # here is some kind of extra
        # keyword
        #ekey = 'DLD1P_' + ident + '_PRANGE'
        opt_keys.append('DLD1P_' + ident + '_PRANGE')

        # initialize the global keylist
        # and the list with indices to be deleted
        bkeys  = []
        dindex = []

        # go over the keylist read in,
        # keeping and index variable
        iindex = 0
        nfound = 0
        for key in keylist:

            # identify the current keyword in the
            # list of possible ones
            if key.keyword in id_keys:

                # store the index
                dindex.append(iindex)

                # create and append the new keyword
                bkeys.append(ConfKey(key.keyword,
                                     key.keyvalue, key.comment))

                # enhance the nuber of keywords found
                nfound += 1

            elif key.keyword in opt_keys:
                # store the index
                dindex.append(iindex)

                # create and append the new keyword
                bkeys.append(ConfKey(key.keyword,
                                     key.keyvalue, key.comment))

            # enhance the index
            iindex += 1

        # check whether all keywords were found
        if nfound < len(id_keys):
            # raise an exeption if not
            raise CKeyNotFound('general')


        # delete the input keywords which
        # have been 'used'
        dindex.sort()
        dindex.reverse()
        for iindex in dindex:
            del keylist[iindex]

        # return the list of global keys
        return bkeys

    def _get_bkey_index(self, keyword):
        """
        Retrieve the index of a beam keyword

        The method searches for the index of
        a requested keyword in the list of beam
        keywords. If the keyword does not exists,
        the index -1 is returned

        @param keyword: name of the requested keyword
        @type keyword: String

        @return: the index of the keyword
        @rtype: integer
        """
        # initialize the return value
        bindex = -1

        # go over all keys
        for index in range(len(self.beamkeys)):
            # check whether the current key matches
            if self.beamkeys[index].keyword == keyword:
                # return it if it matches
                return index

        # return the default
        return bindex

    def get_bkey(self, keyword):
        """
        Retrieve a requested beam keyword

        The method searches the list of beam keywords
        for a fitting keyword. In case that the requested
        keyword exists, it is returned.
        If not 'None' is returned

        @param keyword: name of the requested keyword
        @type keyword: String

        @return: the requested keyword or 'None'
        @rtype: ConfKey
        """
        # initialize the return value
        rkey = None

        # search for the index in the keyword list
        index = self._get_bkey_index(keyword)

        # ckeck whehter the keyword exists
        if index > -1:
            # return the keyword
            return self.beamkeys[index]
        else:
            # return the default
            return rkey

    def get_bvalue(self, keyword):
        """
        Retrieve a requested beam-keyword value

        The method returns the value of the keyword
        which matches the requested value.
        If there is no matching keyword, 'None'
        is returned.

        @param keyword: name of the requested keyword
        @type keyword: String

        @return: the requested keyvalue or 'None'
        @rtype: String
        """
        # set the default return value
        rvalue = None

        # search for the keyword
        key = self.get_bkey(keyword)

        # check whether it is non-NULL
        if key:
            # extract the value
            rvalue = key.keyvalue

        # return the value
        return rvalue

    def check_files(self):
        """
        Checks whether all files exist

        The method checks whether the files whose names
        are within the class data do exist or not.
        An error is reported in case that the files
        do not exist.

        """
        n_sens=0
        
        # list of the root of all
        # beamword keys indicating a file
        fkeys = ['SENSITIVITY_']

        # append the beam identifier to the
        # keyword roots to get the full keyname
        for key in fkeys:
            full_keyword = key + self.ident

            # go over all beam keys
            for bkey in self.beamkeys:

                # check whether the current keyword is right
                # and whether the keyvalue is not 'None'
                if bkey.keyword == full_keyword and bkey.keyvalue.upper() != 'NONE':
                    # check for the file
                    if not os.path.isfile(axeutils.getCONF(bkey.keyvalue)):
                        # report an error
                        err_msg = 'The file: %s does not exist!' % axeutils.getCONF(bkey.keyvalue)
                        raise aXeError(err_msg)
                    else:
                        n_sens += 1
                    
        return n_sens

class TwoDimPolyN(object):
    """
    Object for a polynomial with 2D variance
    """
    def __str__(self):
        """
        String method for the class

        The method transforms the 2D polynomial
        object into its string representation.

        @return: string representation of the object
        @rtype: string
        """
        # initialize the return string
        rstring = str(self.norder)

        for key in self.twodkeys:
            rstring += str(key)

        # return the total string
        return rstring

    def __getitem__(self, index):
        """
        Getindex method for the class

        The operator method which is called
        when an index is requested on a
        class instace
        test = kkk[0]

        @param index: the index to address
        @type index: int
        @return: the indexed object
        @rtype: ConfListKey
        """
        # check whether the index exists
        if index > len(self.twodkeys)-1:
            # raise an exception
            err_msg = 'Index: ' + str(index) + ' does not exist!'
            raise aXeError(err_msg)

        # return the indexed object
        return self.twodkeys[index]

    def __setitem__(self, index, obj):
        """
        Setindex method for the class

        The operator method which is called
        when the index of a class instance is
        set to a value.
        kkk[0] = test

        @param index: the index to address
        @type index: int
        @param obj: description of the object content
        @type obj: ConfListKey
        """
        # check whether the index exists
        if index > len(self.twodkeys)-1:
            # raise an exception
            err_msg = 'Index ' + str(index) + ' does not exist!'
            raise aXeError(err_msg)
        # check whether the input type is correct
        elif type(obj) !=  type(self[0]):
            # raise an exception
            err_msg = 'Object: ' + str(obj) + ' has wrong type: ' +str(type(obj))+'!'
            raise aXeError(err_msg)

        # set the index to the input object
        self.twodkeys[index] = obj

    def _find_order(self, prefix, ident, keylist):
        """
        Find the keyword with the polynomial order

        The method finds and extracts the keyword
        indicating the polynomial degree from
        a keyword list. The keyword is returned.

        @param prefix: keyword prefix
        @type prefix: string
        @param ident: beam identification
        @type ident: character
        @param keylist: list of keywords
        @type keylist: [ConfKey]

        @return: keyword with number of orders
        @rtype: ConfKey
        """
        # create the name of the keyword with the
        # polynomial order
        order_key = prefix + 'ORDER_' + ident

        # extract and return the keyword from the
        # keyword list
        return self._find_key(order_key, keylist)

    def _find_twodkeys(self, prefix, ident, keylist):
        """
        Find the all 2D polynomial keywords

        Given a prefix and a beam identifier the method
        extracts all orders of the 2D polynomial which
        describes the trace or dispersion. The number
        of orders expected is taken from the object data.

        @param prefix: keyword prefix
        @type prefix: string
        @param ident: beam identification
        @type ident: character
        @param keylist: list of keywords
        @type keylist: [ConfKey]

        @return: list of keywords
        @rtype: [ConfKey]
        """

        # initialize an empty list
        twodkeys = []

        # for each expected keyword
        for ii in range(int(self.norder.keyvalue)+1):
            # form the keyword name
            twodkey = prefix + ident + '_' + str(ii)

            # extract the new keyword
            newkey = self._find_key(twodkey, keylist, 1)

            if self._check_twodkey(newkey):
                #extract the keyword and append it to the list
                twodkeys.append(newkey)
            else:
                raise CKeyLengthWrong(ident, twodkey)

        # return the list
        return twodkeys

    def _find_key(self, keyword, keylist, lkey=0):
        """
        Extract a certain keyword from the list

        The methods searches for a particular keyword
        in a keyword list. If found, the keyword is
        copied and destroied in the input list.
        If not found, an exception is fired.

        @param keyword: the keyword name
        @type keyword: string
        @param keylist: list of keywords
        @type keylist: [ConfKey]

        @return: the extracted keyword
        @rtype: ConfKey
        """

        # initialize the index
        iindex = 0

        # set indicator to "not found"
        found = -1

        # go over all keys in the list
        for key in keylist:

            # checke whether the keyword is the desired one
            if key.keyword == keyword:
                # create a list keyword if desired
                if lkey:
                    nkey = ConfListKey(key.keyword, key.keyvalue, key.comment)
                else:
                    nkey = ConfKey(key.keyword, key.keyvalue, key.comment)

                # store the index
                found = iindex

            # enhance the index
            iindex += 1

        # fire an exception if nothing was found
        if found < 0:
            raise CKeyNotFound(keyword)
        # delete the keyword from the inlist
        else:
            del keylist[found]

        # return the keyword
        return nkey

    def _check_twodkey(self, inkey):
        """
        Check the length of the a field dependent keyword

        Field dependent keywords such as the polynimial
        coefficients in the trace description and dispersion
        solution must have a certain number of values,
        which is:
        n = m^2/2 + m/2
        The method checks whether the number of values
        is in agreement with this.

        @param inkey: the keyword name
        @type inkey: ConfListKey

        @return: 1/0
        @rtype: int
        """

        # determine the length of the list
        n = float(len(inkey.kvallist))

        # compute the 'order' of the xy-dependence
        m = (-1.0 + math.sqrt(1.0+8.0*n))/2.0

        # chech whether the 'order' is integer
        if math.fabs(m-int(m)) > 1.0e-16:
            # no integer -> key length wrong
            return 0

        # integer -> key length correct
        return 1

    def str_header(self, description):
        """
        Create a header string

        The method offers to the subclasses the possibility
        to have a meaningful string header before the
        actual data string.

        @param description: description of the object content
        @type description: string
        @return: the header string
        @rtype: string
        """
        # pre-decoration
        rstring  = '\n#\n# '

        # add description
        rstring += description

        # add post-decoration
        rstring += ':\n#\n'

        # return the result
        return rstring


class ConfigTrace(TwoDimPolyN):
    """
    Configuration Beam object
    """
    def __init__(self, ident=None, keylist=None):
        """
        Initializer for the configuration beam object

        The method initializes a configuration beam
        object for a given beam identifier.
        All necessary keywords are extracted from
        an input keyword list.
        In case of missing keywords an exception
        is fired.

        @param ident: beam identification
        @type ident: Character
        @param keylist: list of keywords
        @type keylist: [ConfKey]
        """

        # try to read in the keywords
        try:
            self.ident = ident
            self.norder  = self._find_order('DYDX_', ident, keylist)
            self.twodkeys = self._find_twodkeys('DYDX_', ident, keylist)

        # raise an exception if keywords are missing
        except CKeyNotFound as e:
            raise TraceNotFound(ident, e.keyword)
        except CKeyLengthWrong as e:
            print('Field dependent keyword: ' + e.keyword)

    def __str__(self):
        """
        String method for the class

        @return: string representation of the object
        @rtype: string
        """
        # create the label or description
        description = 'Trace description for Beam ' + str(self.ident)

        # get the string header
        rstring = super(ConfigTrace, self).str_header(description)

        # get the data string
        rstring += super(ConfigTrace, self).__str__()

        # return the result
        return rstring

class ConfigDisp(TwoDimPolyN):
    """
    Configuration Beam object
    """
    def __init__(self, ident=None, keylist=None):
        """
        Initializer for the configuration dispersion object

        The method initializes a configuration dispersion
        object for a given beam identifier.
        All necessary keywords are extracted from
        an input keyword list.
        In case of missing keywords an exception
        is fired.

        @param ident: beam identification
        @type ident: Character
        @param keylist: list of keywords
        @type keylist: [ConfKey]
        """
        # try to read in the keywords
        try:
            self.ident = ident
            self.norder   = self._find_order('DISP_', ident, keylist)
            self.twodkeys = self._find_twodkeys('DLDP_', ident, keylist)
        # raise an exception if keywords are missing
        except CKeyNotFound as e:
            try:
                self.twodkeys = self._find_twodkeys('DLD1P_', ident, keylist)
            # raise an exception if keywords are missing
            except CKeyNotFound as e:
                raise DispNotFound(ident, e.keyword)
            except CKeyLengthWrong as e:
                print('\nField dependent keyword: ' + e.keyword + ' has wrong length!')
                raise DispNotFound(ident, e.keyword)
        except CKeyLengthWrong as e:
            print('\nField dependent keyword: ' + e.keyword + ' has wrong length!')
            raise DispNotFound(ident, e.keyword)

    def __str__(self):
        """
        String method for the class

        @return: string representation of the object
        @rtype: string
        """
        # create the label or description
        description = 'Dispersion solution for Beam ' + str(self.ident)

        # get the string header
        rstring = super(ConfigDisp, self).str_header(description)

        # get the data string
        rstring += super(ConfigDisp, self).__str__()

        # return the result
        return rstring


class DefConfHeader(object):
    """
    Default header for a configuration file
    """
    def __init__(self):
        """
        Constructor for the default config file
        """
        self.header = []
        self.header.append(
"""#-----------------------------------------------------------
#
# Default configuration file for aXe
#
#-----------------------------------------------------------""")
    def __str__(self):
        """
        String method for the header class

        @return: string representation of the object
        @rtype: string
        """
        rstring = ''
        for line in self.header:
            rstring = rstring + line

        return rstring

class ConfHeader(DefConfHeader):
    """
    Header class for the configuration file
    """
    def __init__(self, filename=None):
        """
        Initializes the configuration header class

        The method extracts the header from a configuration
        file. If no filename is provided, a default
        header is created.

        @param filename: name of the configuration file
        @type filename: string
        """
        # no filename -> default header
        if filename is None:
            super(ConfHeader, self).__init__()
        else:
            # initialize the data list
            self.header = []

            # intialize the start pointer
            start = 1

            # open and parse through the file
            fopen = file(filename, 'r')
            for line in fopen:

                # check whether the sart pointer is still set
                if start:

                    # strip the line
                    str_line = line.strip()

                    # check whether the first character
                    # is a comment, which qualifies
                    # the line as part of the header
                    if len(str_line) > 0 and str_line[0] == '#':
                        # append the line to the header data
                        self.header.append(line.strip()+'\n')
                    else:
                        # set the starter pointer to 0,
                        # thus indicating the end of the header
                        start=0
            # close the file
            fopen.close


class ConfKey(object):
    """
    Class for a keyword in a configuration file

    This keyword class is a light, but yet versatile
    and important class to strore a keyword entry in a
    configuration file. All important values are
    direct ly read from the object attributes.
    """
    def __init__(self, keyword, keyvalue, comment=None):
        """
        Constructor for the keyword class

        Initializer for the keyword class.
        The keyword instance is created using
        all input values.

        @param keyword: the keword name
        @type keyword: string
        @param keyvalue: the keyword value
        @type keyvalue: string
        @param comment: the keyword comment
        @type comment: string
        """
        self.keyword  = keyword
        self.keyvalue = keyvalue
        self.comment  = comment

    def __str__(self):
        """
        String method for the class

        The method creats and returns
        the string representation of the
        keyword.

        @return: string representation of the object
        @rtype: string
        """
        rstring = self.keyword + ' ' + str(self.keyvalue)
        if self.comment != None:
            rstring = rstring + ' ; ' + self.comment

        rstring += '\n'

        return rstring

class ConfListKey(ConfKey):
    """
    Class for a keyword list

    The keyword list class is a subclass derived from the
    keyword class. In the keyword list class has as an
    additional attribute the keyvalues transformed to a list
    of floats.
    """
    def __init__(self, keyword, keyvalue, comment=None):
        """
        Constructor for the keyword list class

        Initializer for the keyword list class.
        The keyword instance is created using
        all input values.

        @param keyword: the keword name
        @type keyword: string
        @param keyvalue: the keyword values
        @type keyvalue: string
        @param comment: the keyword comment
        @type comment: string
        """
        # initialize the keyvalue list
        self.kvallist = []

        # create a traditional keyword instance
        super(ConfListKey, self).__init__(keyword, keyvalue, comment)

        # split the string keyvalue
        vlist = self.keyvalue.split()
        for value in vlist:
            # append the floats to the list
            self.kvallist.append(float(value))

    def __getitem__(self, index):
        """
        Getindex method for the class

        The operator method which is called
        when an index is requested on a
        class instace
        test = kkk[0]

        @param index: the index to address
        @type index: int
        @return: the indexed object
        @rtype: float
        """
        # check whether the index exists
        if index > len(self.kvallist)-1:
            # raise an exception
            err_msg = 'Index: ' + str(index) + ' does not exist!'
            raise aXeError(err_msg)

        # return the indexed object
        return self.kvallist[index]

    def __setitem__(self, index, obj):
        """
        Setindex method for the class

        The operator method which is called
        when the index of a class instance is
        set to a value.
        kkk[0] = test

        @param index: the index to address
        @type index: int
        @param obj: description of the object content
        @type obj: ConfListKey
        """
        # check whether the index exists
        if index > len(self.kvallist)-1:
            # raise an exception
            err_msg = 'Index ' + str(index) + ' does not exist!'
            raise aXeError(err_msg)
        # check whether the input type is correct
        elif type(obj) !=  type(self[0]):
            # raise an exception
            err_msg = 'Object: ' + str(obj) + ' has wrong type: ' +str(type(obj))+'!'
            raise aXeError(err_msg)

        # set the index to the input object
        self.kvallist[index] = obj

    def __str__(self):
        """
        String method for the class

        The method creats and returns
        the string representation of the
        keyword.

        @return: string representation of the object
        @rtype: string
        """
        # first comes the keyword
        rstring = self.keyword

        # append the keyvalues using a default format
        for value in self.kvallist:
            rstring = rstring + ' %12.6g' % value

        # append the comment
        if self.comment != None:
            rstring = rstring + ' ; ' + self.comment

        # append a linefeed
        rstring += '\n'

        # return the complete string
        return rstring

class ConfError(Exception):
    """
    Base class for exceptions in this module
    """
    pass

class CKeyNotFound(ConfError):
    """
    Error for missing keyword
    """
    def __init__(self, keyword):
        self.keyword = keyword

class BeamNotFound(ConfError):
    """
    Error for unknown beam
    """
    def __init__(self, ident):
        self.ident = ident

class TraceNotFound(ConfError):
    """
    Error for unknown trace
    """
    def __init__(self, ident, keyword=None):
        self.ident   = ident
        self.keyword = keyword

class DispNotFound(ConfError):
    """
    Error for unknown dispersion
    """
    def __init__(self, ident, keyword=None):
        self.ident   = ident
        self.keyword = keyword

class CKeyLengthWrong(ConfError):
    """
    Error for wrong lengt in KeywordList
    """
    def __init__(self, ident, keyword=None):
        self.ident   = ident
        self.keyword = keyword
