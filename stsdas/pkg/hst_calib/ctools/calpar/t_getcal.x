include "calpar.h"

#---------------------------------------------------------------------------
.help getcal Jul97 source
.ih
NAME
getcal -- Retrieve value of keywords from an image and place them into pset.
.endhelp
#---------------------------------------------------------------------------
procedure t_getcal

#
#  07 July 1997  M. De La Pena  Modified to ensure that if the input image
#                               is a FITS file, only the PHU is updated.
#  10 July 1997  M. De La Pena  Substantial mods to accommodate different PSETs
#                               for different detectors/observing modes of STIS.

# Declarations
int     i, j, k                 # Loop indices.
int     instr                   # Instrument index.
int     locext                  # Index position for filename "FITS" match.
int     locmama                 # Index position for "MAMA" match.
int     nextn                   # Number of characters in the extension.

bool    found                   # TRUE if a parameter is in the pset.
bool    det_found               # TRUE if DETECTOR is in pset (STIS only).
bool    obs_found               # TRUE if OBSTYPE is in pset (STIS only).

pointer contents                # Pset contents descriptor.
pointer image                   # Image descriptor.
pointer instr_name              # Instrument name.
pointer det_name                # Detector name (STIS only).
pointer obs_name                # Observing mode (STIS only).
pointer tmp_name                # Temporary string.
pointer list                    # Image list descriptor.
pointer pset_name               # Pset name.
pointer sp                      # Stack pointer
pointer xstr, ystr              # Generic strings.
pointer tmpxstr                 # Temporary string.

string  keywords_str    "keywords"
string  input_str       "input"
string  instrpset_warn  "WARNING: getcal: No %s parameter in pset %s, assuming correct pset\n"
string  instrument_str  "INSTRUME"
string  det_strg        "DETECTOR"
string  obs_strg        "OBSTYPE"
string  match_err       "getcal: Pset instrument - %s - does not match image instrument - %s"
string  stdmatch_err    "getcal: Pset detector - %s - does not match image detector - %s."
string  stomatch_err    "getcal: Pset obstype - %s - does not match image obstype - %s."
string  stmiss_err     "getcal: Pset parameter %s or %s missing."
string  noinstr_warn    "WARNING: getcal: Keyword %s not in image, no checks will occur.\n"
string  nokeyword_err   "getcal: Keyword %s doesn't exist\n"
string  space           " "
string  unknowninstr_err "getcal: Undefined or unknown instrument %s"

# Functions.
int     imaccf(), imtgetim(), strdic(), strlen(), strsearch()
int     fnextn()
bool    streq()
pointer cp_open(), immap(), imtopen()

errchk  clgstr, clpstr, cp_close, cp_open, cp_write, eprintf
errchk  imaccf, imgstr, immap, imunmap
errchk  salloc, sfree, smark, sprintf


begin

        call smark (sp)
        call salloc (instr_name, SZ_LINE, TY_CHAR)
        call salloc (det_name, SZ_LINE, TY_CHAR)
        call salloc (obs_name, SZ_LINE, TY_CHAR)
        call salloc (tmp_name, SZ_LINE, TY_CHAR)
        call salloc (pset_name, SZ_LINE, TY_CHAR)
        call salloc (xstr, SZ_LINE, TY_CHAR)
        call salloc (ystr, SZ_LINE, TY_CHAR)
        call salloc (tmpxstr, SZ_LINE, TY_CHAR)

        # Retrieve the image name and open.  The input may be an image
        # list.  If so, take only the first name.
	call clgstr (input_str, Memc[xstr], SZ_LINE)
        list = imtopen (Memc[xstr])
        if (imtgetim (list, Memc[xstr], SZ_LINE) == EOF)
            call error (1, "getcal: no input file specified")
        call imtclose (list)

        # Check if the image is a FITS file.  If it is, ensure that only 
        # the PHU extension has been appended to the file name for proper
        # acquisition of information.
        call imgcluster (Memc[xstr], Memc[xstr], SZ_LINE)
        nextn  = fnextn (Memc[xstr], Memc[tmpxstr], SZ_LINE)
        call strupr (Memc[tmpxstr])
        locext = strsearch (Memc[tmpxstr], "FITS")
        if (locext != 0) 
            call strcat ("[0]", Memc[xstr], SZ_LINE)

        # Open the image file.
	image = immap (Memc[xstr], READ_WRITE, 0)

        # Set the appropriate pset.  If a pset is specified, use it, see if
        # the INSTRUMENT parameter is in it.  If so, and the value of
        # INSTRUMENT is the same as the instrument of the image, then use it.
        # If INSTRUMENT is not the same, error out.  If INSTRUMENT is not a
        # parameter, give a warning but continue on.  If the image doesn't have
        # any instrument specification and no pset is specified, error out.
        # If a pset is specified, again give a warning and continue on.

        # Get pset name and instrument of image.
        call clgstr (keywords_str, Memc[pset_name], SZ_LINE)
        iferr (call imgstr (image, instrument_str,
                            Memc[instr_name], SZ_LINE)) {
            call eprintf (noinstr_warn)
            call pargstr (instrument_str)
            Memc[instr_name] = EOS
        }
        call strlwr (Memc[instr_name])
        instr = strdic (Memc[instr_name], Memc[xstr], SZ_LINE, INSTR_DICT)

        # If STIS, also obtain the DETECTOR and OBSTYPE keywords of image.
        if (instr == STIS) {
            iferr (call imgstr (image, det_strg,
                                Memc[det_name], SZ_LINE)) {
                call eprintf (noinstr_warn)
                call pargstr (det_strg)
                Memc[det_name] = EOS
            }

            iferr (call imgstr (image, obs_strg,
                                Memc[obs_name], SZ_LINE)) {
                call eprintf (noinstr_warn)
                call pargstr (obs_strg)
                Memc[obs_name] = EOS
            }

            call strlwr (Memc[det_name])
            call strlwr (Memc[obs_name])
        }
        
        # If a pset is not specified, try to determine it from the instrument.
        contents = NULL
        if (strlen (Memc[pset_name]) <= 0) {
            switch (instr) {
            case FOC:
                call strcpy (STD_FOC_PAR, Memc[pset_name], SZ_LINE)
            case FOS:
                call strcpy (STD_FOS_PAR, Memc[pset_name], SZ_LINE)
            case HRS:
                call strcpy (STD_HRS_PAR, Memc[pset_name], SZ_LINE)
            case HSP:
                call strcpy (STD_HSP_PAR, Memc[pset_name], SZ_LINE)
            case NICMOS:
                call strcpy (STD_NICMOS_PAR, Memc[pset_name], SZ_LINE)
            case WFPC2:
                call strcpy (STD_WFPC2_PAR, Memc[pset_name], SZ_LINE)
            case WFPC:
                call strcpy (STD_WFPC_PAR, Memc[pset_name], SZ_LINE)
            case STIS:
                call whpset (Memc[det_name], Memc[obs_name], Memc[pset_name])
            case ACS:
                if (streq(Memc[det_name],SBC))
                    call strcpy (STD_ACSSBC_PAR, Memc[pset_name], SZ_LINE)
                else
                    call strcpy (STD_ACSCCD_PAR, Memc[pset_name], SZ_LINE)
            default:
                call sprintf (Memc[xstr], SZ_LINE, unknowninstr_err)
                call pargstr (Memc[instr_name])
                call error (1, Memc[xstr])
            }
        }

        # Else, a pset was specified.  Handle accordingly.
        else {

            # See if there is an instrument parameter in the pset.  If not,
            # give a warning and assume the pset is OK.
            contents = cp_open (Memc[pset_name])
            found = false
            do i = 1, CP_NPARAMS(contents) {
                found = streq (INSTR_PAR, CP_PAR(contents,i))
                if (found)
                    break
            }
            if (!found) {
                call eprintf (instrpset_warn)
                call pargstr (INSTR_PAR)
                call pargstr (Memc[pset_name])
            }
            
            # Else, check instruments.  If not the same, then error out.
            else {
                if (instr != NO_INSTR &&
                    !streq (CP_VALUE(contents,i), Memc[instr_name])) {
                    call sprintf (Memc[xstr], SZ_LINE, match_err)
                    call pargstr (CP_VALUE(contents,i))
                    call pargstr (Memc[instr_name])
                    call error (1, Memc[xstr])
                }
            }

            # For STIS, also check detector and obstype.  If not the same
            # or missing, then error out.
            if (instr == STIS) {
                det_found = false
                obs_found = false
                do k = 1, CP_NPARAMS(contents) {
                    det_found = streq (DETECTOR_PAR, CP_PAR(contents,k))
                    if (det_found)
                        break
                }
                do j = 1, CP_NPARAMS(contents) {
                    obs_found = streq (OBSTYPE_PAR, CP_PAR(contents,j))
                    if (obs_found)
                        break
                }

                # Keywords detector and/or obstype are missing.
                if (!det_found || !obs_found) {
                    call sprintf (Memc[xstr], SZ_LINE, stmiss_err)
                    call pargstr (DETECTOR_PAR)
                    call pargstr (OBSTYPE_PAR)
                    call error (1, Memc[xstr])
                }
 
                # Else, check to make sure that the OBSTYPE and DETECTOR
                # match.  If not the same, then error out.
                else {

                    # Check obstype keyword.
                    if (!streq (CP_VALUE(contents,j), Memc[obs_name])) {
                        call sprintf (Memc[xstr], SZ_LINE, stomatch_err)
                        call pargstr (CP_VALUE(contents,j))
                        call pargstr (Memc[obs_name])
                        call error (1, Memc[xstr])
                    }

                    # Both STIS MAMAs use the same psets.  Strip the prefix
                    # (FUV- or NUV-) before comparing to the pset detector
                    # contents defined as just "mama".
                    locmama = strsearch (Memc[det_name], "mama")
                    if (locmama != 0) 
                        call strcpy ("mama", Memc[tmp_name], SZ_LINE)
                    else
                        call strcpy (Memc[det_name], Memc[tmp_name], SZ_LINE)
                 
                    # Check detector keyword.
                    if (!streq (CP_VALUE(contents,k), Memc[tmp_name])) {
                        call sprintf (Memc[xstr], SZ_LINE, stdmatch_err)
                        call pargstr (CP_VALUE(contents,k))
                        call pargstr (Memc[tmp_name])
                        call error (1, Memc[xstr])
                    }
                }
            }
            
        }

        # Open the pset and retrieve its contents.
        call clpstr (keywords_str, Memc[pset_name])
        if (contents == NULL)
            contents = cp_open (Memc[pset_name])

        # For each parameter, retrieve the keyword value from the image
        # and write it to the pset.
        do i = 1, CP_NPARAMS(contents) {
            
            # Check for unwanted parameters.
            if (strdic (CP_PAR(contents,i), Memc[xstr], SZ_LINE,
                        BAD_PARS) > 0)
                next
            
            if (imaccf (image, CP_PAR(contents,i)) == YES)
                call imgstr (image, CP_PAR(contents,i), Memc[xstr], SZ_LINE)
            else {
                call eprintf (nokeyword_err)
                call pargstr (CP_PAR(contents,i))
                call strcpy (space, Memc[xstr], SZ_LINE)
            }

            # If this is an uppercase value, change it to lower case.
            call strcpy (Memc[xstr], Memc[ystr], SZ_LINE)
            call strlwr (Memc[ystr])
            if (strdic (Memc[ystr], Memc[ystr], SZ_LINE, UPPER_STR_DIC) > 0)
                call strcpy (Memc[ystr], Memc[xstr], SZ_LINE)
            
            call strcpy (Memc[xstr], CP_VALUE(contents,i), CP_VALUE_SIZE)
        }
        call cp_write (contents)

        # That's all folks.
        call cp_close (contents)
	call imunmap (image)
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_getcal
#---------------------------------------------------------------------------
procedure whpset (det_name, obs_name, pset_name)

# Declarations
char det_name[SZ_LINE]               # STIS detector.
char obs_name[SZ_LINE]               # STIS observation type.
char pset_name[SZ_LINE]              # Pset for detector and observation type.

pointer tmpstr                       # Temporary string.

pointer sp                           # Stack pointer.

string  detunknown_err   "getcal: Undefined or unknown detector %s"
string  obsunknown_err   "getcal: Undefined or unknown obstype %s"

# Functions.
bool    streq()

begin
        call smark (sp)
        call salloc (tmpstr, SZ_LINE, TY_CHAR)

        # First, determine which detector is appropriate, then determine
        # the observation type.  If unknown detector or observation type, 
        # error out.
        if (streq(det_name, CCD)) {
            if (streq(obs_name, SPECTROSCOPIC)) 
                call strcpy (STD_STISCS_PAR, pset_name, SZ_LINE)
            else if (streq(obs_name, IMAGING))
                call strcpy (STD_STISCI_PAR, pset_name, SZ_LINE)
            else {
                call sprintf (Memc[tmpstr], SZ_LINE, obsunknown_err)
                call pargstr (obs_name)
                call error (1, Memc[tmpstr])
            }
        }
        else if ((streq(det_name, FUV_MAMA)) || (streq(det_name, NUV_MAMA))) {
            if (streq(obs_name, SPECTROSCOPIC))
                call strcpy (STD_STISMS_PAR, pset_name, SZ_LINE)
            else if (streq(obs_name, IMAGING))
                call strcpy (STD_STISMI_PAR, pset_name, SZ_LINE)
            else {
                call sprintf (Memc[tmpstr], SZ_LINE, obsunknown_err)
                call pargstr (obs_name)
                call error (1, Memc[tmpstr])
            }
        }
        else {
            call sprintf (Memc[tmpstr], SZ_LINE, detunknown_err)
            call pargstr (det_name)
            call error (1, Memc[tmpstr])
        }
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of whpset
#---------------------------------------------------------------------------
