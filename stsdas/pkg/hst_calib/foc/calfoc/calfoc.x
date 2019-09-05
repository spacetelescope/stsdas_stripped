include	<imhdr.h>
include	<imio.h>
include	<ctype.h>
include "calfoc.h"

define	VERSION		"1.3.5.0"	# calfoc version number

# size of the string for path info for phot
define	SZ_PHOT_PATH	1000

# Maximum size of a line of text for messages.
define	SZ_PODPS_LINE	79


# CALFOC -- Processes an FOC image in the RSDP pipeline.
#  This takes an input image and calibration images and does
#  the simple processing with any combination of the dark, itf, uni, geo.
#
# D. Giaretta March 1988
# Yuri Frankel & Phil Hodge	October 1988  modified
# Phil Hodge	2-June-1989	call cal_g_phot, etc; change error handling
# Phil Hodge	23-Oct-1989	change ITF file from 3-D to 2-D
# Phil Hodge	29-Nov-1989	write calfoc version number to stdout
# Phil Hodge	 8-Feb-1990	If there's nothing to do, don't do anything;
#				call xer_reset in xc_error.
# Phil Hodge	 3-Oct-1990	Dezoom CD matrix & CRPIX values; if EXPTIME
#				is negative, set it to zero; use UNIHFILE if
#				it's different from "N/A" or "FNF".
# Phil Hodge	11-Oct-1990	Call newgeom (by Jedrzejewski & Sparks)
#				instead of xcgeocoord & xcgeoco.
# Phil Hodge	17-Jan-1991	get_cal_files:  use dznaxis1 for np_section.
# Phil Hodge	19-Mar-1991	xcalimg:  call imunmap (p_im) after GEO corr.
# Phil Hodge	20-Mar-1991	Flip SAMPBEG in x_c_foc_immap, assuming that
#				header parameter is original value; delete
#				references to FOC_SOFF, FOC_INC (i.e. can't
#				give image section as input); delete logerr.
# Phil Hodge	13-May-1991	Call xc_group to write rootname.grp file.
# Phil Hodge	19-Jul-1991	Change extension from .grp to .cgr.
# Phil Hodge	29-Jul-1991	Do geo correction before uni.
# Phil Hodge	10-Jul-1992	Call error if mismatch between mode (normal
#				or specto) and unicorr or sdecorr; another
#				argument to xc_group.
# Phil Hodge	25-Jan-1993	Extract logmsg to a separate file.
# Phil Hodge	24-Mar-1994	Check PEDIGREE in reference images.
# Phil Hodge	11-Apr-1994	Set scale for output CD matrix if GEO corr.;
#				add COSTAR to PHOTMODE if KXDEPLOY is true.
# Phil Hodge	31-May-1994	Include aperture name in mode string.
# P. Greenfield  5-Apr-1996	Add exit status return for host tasks.
procedure t_calfoc()

char	infile[SZ_FNAME]		# name or root name of input image
char	outfile[SZ_FNAME]		# root name of output image
#--

pointer in_ptr			# imhdr pointer for input image
pointer	i_foc, x_c_foc_immap(), caldat
pointer sp
pointer mess			# scratch for message
pointer phot_path		# scratch for path info for phot

char	modestr[SZ_FNAME]	# mode, e.g. "FOC F/48 SPEC"
char	dot			# '.'
real	phot[4]			# flambda, zero pt, pivot wl, rms width
int	dotpos			# location of '.' in file name
int	stridx(), strldx(), strlen(), strncmp()

char	rootname[SZ_FNAME]	# root name of the observation
common	/xcroot/ rootname	# pass to xc_error, c_error, logmsg

begin
	# Setup error handler to return status if run from host.
	call post_exit_handler

	# Write a message giving the task name and version number.
	call printf ("*** CALFOC - Version %s ***\n")
	    call pargstr (VERSION)
	call flush (STDOUT)

	call smark(sp)
	call salloc (mess, SZ_FNAME, TY_CHAR)
	call salloc (phot_path, SZ_PHOT_PATH, TY_CHAR)
	call salloc( caldat, SZ_CALDATA, TY_STRUCT )
	call salloc( i_foc, SZ_FOC_COORD, TY_STRUCT)

	rootname[1] = EOS		# initial value

	# Get the names of the input and output files.  If no output file name
	# was specified, copy input to output.  Remove any extension from the
	# output file name.
        call clgstr ("input", infile, SZ_FNAME)
        call clgstr ("output", outfile, SZ_FNAME)
	if (outfile[1] == EOS)
	    call strcpy (infile, outfile, SZ_FNAME)
	dot = '.'
	dotpos = strldx (dot, outfile)			# find last '.'
	if (dotpos > 0)
	    outfile[dotpos] = EOS			# remove extension

	# Append extension to name of input file if no extension was given.
	if (stridx (dot, infile) < 1)
	    call strcat (INPUTEXT, infile, SZ_FNAME)

	# We don't have rootname yet.
	# Print a time-stampted message giving the name of the input file.
	# Also print the name of the output file if it's different.
	call sprintf (Memc[mess], SZ_FNAME, "Processing image %s")
	    call pargstr (infile)
	call log_progress (Memc[mess])
	dotpos = strlen (outfile)
	if (strncmp (infile, outfile, dotpos) != 0) {
	    call printf ("  output root name %s\n")
		call pargstr (outfile)
	    call flush (STDOUT)
	}

	# Initialise the caldat structure.
	call x_init_caldat (caldat)

	# open the input image
	in_ptr = x_c_foc_immap( infile, i_foc, READ_ONLY, NULL)

	# Get the root name of the observation.  This name will be included
	# in all messages.
	iferr (call imgstr (in_ptr, "rootname", rootname, SZ_FNAME))
	    rootname[1] = EOS

	# get the details of the calibration files
	call get_cal_files( i_foc, caldat, phot, modestr, Memc[phot_path])

	# write some info to the log file about the image
	call log_image (i_foc)		# Moved to here by Y.F.

	call imunmap (in_ptr)		# reopened later

	if (DO_SOMETHING(caldat)) {

	    call xcalimg (infile, outfile, caldat,
			phot, modestr, Memc[phot_path])

	} else {

	    # If we're not going to do any calibration, then we must write
	    # the rootname.cgr file using the values in the input image.
	    call xc_group (infile, infile, outfile)

	    # If we're not doing any calibration because of dummy reference
	    # images, we should log that fact to the trailer.
	    call x_bac_log (caldat)
	    call x_itf_log (caldat)
	    call x_pxl_log (caldat)
	    call x_abs_log (caldat)
	    call x_geo_log (caldat)
	    call x_uni_log (caldat)
	    call x_sde_log (caldat)

	    call logmsg ("Info:  No calibration done.")
	}

	call clos_cal_files (caldat)

	call sfree (sp)
end

# x_init_caldat -- assign initial values in the caldat structure

procedure x_init_caldat (caldat)

pointer caldat		# i: calibration info structure
#--

begin
	# These are pointers to the imhdr structure.
	BAC_FILE(caldat) = NULL
	ITF_FILE(caldat) = NULL
	UNI_FILE(caldat) = NULL
	GEO_FILE(caldat) = NULL
	SDE_FILE(caldat) = NULL

	# These are flags specifying which calibration steps should be done.
	DO_BAC(caldat) = false
	DO_ITF(caldat) = false
	DO_PXL(caldat) = false
	DO_ABS(caldat) = false
	DO_GEO(caldat) = false
	DO_UNI(caldat) = false
	DO_SDE(caldat) = false

	BAC_FLAG(caldat) = OMIT
	ITF_FLAG(caldat) = OMIT
	PXL_FLAG(caldat) = OMIT
	ABS_FLAG(caldat) = OMIT
	GEO_FLAG(caldat) = OMIT
	UNI_FLAG(caldat) = OMIT
	SDE_FLAG(caldat) = OMIT

	# These are flags specifying to which files (header, trailer)
	# we must log file name and calibration flag.
	BAC_LOG(caldat) = NO_LOG
	ITF_LOG(caldat) = NO_LOG
	PXL_LOG(caldat) = NO_LOG
	ABS_LOG(caldat) = NO_LOG
	GEO_LOG(caldat) = NO_LOG
	UNI_LOG(caldat) = NO_LOG
	SDE_LOG(caldat) = NO_LOG

	# These are pointers to scratch for values of PEDIGREE and DESCRIP.
	call salloc (BAC_PEDIGREE(caldat), SZ_PATHNAME, TY_CHAR)
	call salloc (BAC_DESCRIP(caldat), SZ_PATHNAME, TY_CHAR)
	call salloc (ITF_PEDIGREE(caldat), SZ_PATHNAME, TY_CHAR)
	call salloc (ITF_DESCRIP(caldat), SZ_PATHNAME, TY_CHAR)
	call salloc (UNI_PEDIGREE(caldat), SZ_PATHNAME, TY_CHAR)
	call salloc (UNI_DESCRIP(caldat), SZ_PATHNAME, TY_CHAR)
	call salloc (GEO_PEDIGREE(caldat), SZ_PATHNAME, TY_CHAR)
	call salloc (GEO_DESCRIP(caldat), SZ_PATHNAME, TY_CHAR)
	call salloc (SDE_PEDIGREE(caldat), SZ_PATHNAME, TY_CHAR)
	call salloc (SDE_DESCRIP(caldat), SZ_PATHNAME, TY_CHAR)
	Memc[BAC_PEDIGREE(caldat)] = EOS
	Memc[BAC_DESCRIP(caldat)] = EOS
	Memc[ITF_PEDIGREE(caldat)] = EOS
	Memc[ITF_DESCRIP(caldat)] = EOS
	Memc[UNI_PEDIGREE(caldat)] = EOS
	Memc[UNI_DESCRIP(caldat)] = EOS
	Memc[GEO_PEDIGREE(caldat)] = EOS
	Memc[GEO_DESCRIP(caldat)] = EOS
	Memc[SDE_PEDIGREE(caldat)] = EOS
	Memc[SDE_DESCRIP(caldat)] = EOS
end

# GET_CAL_FILES -- get all the calibration data required - taking
# appropriate sections of full format images.
# This routine also computes the photometry info.

procedure get_cal_files (i_foc, caldat, phot, modestr, phot_path)

pointer	i_foc		# i: input image FOC structure
pointer	caldat		# i/o: cal files info
real	phot[4]		# o: flam, zero pt, pivot wl, rms bandwidth
char	modestr[ARB]	# o: mode, e.g. "FOC F/96 F430W"
char	phot_path[ARB]	# o: path info for phot
#--	
char	section[SZ_FNAME] 	# section string to append to image
int	cal_op_stat()
int	naxis1, naxis2, sampbeg, linebeg
int	relay			# which relay:  either 48 or 96
int	ip
int	files_not_found		# number of reference files not found
int	ctoi()
real	imgetr()
pointer	i_im, immap()
char	tstring[SZ_PATHNAME], text[SZ_PATHNAME]
char	unifile[SZ_PATHNAME]		# name of UNI file
int	imaccess(), imaccf()
int	strncmp()
bool	streq()

begin
	files_not_found = 0		# so far, no file not found

	i_im = FOC_IMPT(i_foc)

	# Set a flag to indicate whether we have a normal image or
	# a long-slit spectrographic image.
	call imgstr (i_im, "smmmode", text, SZ_PATHNAME)

	if (streq (text, "INBEAM"))
	    FOC_IMG_MODE(caldat) = FOC_SPECTRO
	else
	    FOC_IMG_MODE(caldat) = FOC_NORMAL
	
	sampbeg = FOC_SAMPBEG(i_foc)
	linebeg = FOC_LINEBEG(i_foc)
	naxis1  = FOC_NPIX1(i_foc)
	naxis2  = FOC_NPIX2(i_foc)

	# Set flags for operations to be performed.

	if (cal_op_stat (i_im, "pxlcorr") == PERFORM) {
	    DO_PXL(caldat) = true
	    PXL_FLAG(caldat) = PERFORM
	    PXL_LOG(caldat) = LOG_TRAILER		# only log to trailer
	}
	if (DO_PXL(caldat) && ! FOC_ZOOM_MODE(i_foc))
	    call logmsg ("Warning:  image is not zoomed, but pxlcorr=perform")
	else if ( ! DO_PXL(caldat) && FOC_ZOOM_MODE(i_foc))
	    call logmsg ("Warning:  image is zoomed, but pxlcorr=omit")

	if (cal_op_stat( i_im, "baccorr" ) == PERFORM) {
	    DO_BAC(caldat) = true
	    BAC_FLAG(caldat) = PERFORM
	    BAC_LOG(caldat) = LOG_TRAILER + LOG_HEADER
	}

	if (cal_op_stat (i_im, "itfcorr") == PERFORM) {
	    DO_ITF(caldat) = true
	    ITF_FLAG(caldat) = PERFORM
	    ITF_LOG(caldat) = LOG_TRAILER + LOG_HEADER
	}

	if (cal_op_stat (i_im, "wavcorr") == PERFORM) {
	    DO_ABS(caldat) = true
	    ABS_FLAG(caldat) = PERFORM
	    ABS_LOG(caldat) = LOG_TRAILER		# only log to trailer
	}

	if (cal_op_stat (i_im, "geocorr") == PERFORM) {
	    DO_GEO(caldat) = true
	    GEO_FLAG(caldat) = PERFORM
	    GEO_LOG(caldat) = LOG_TRAILER + LOG_HEADER
	}
	
	if (cal_op_stat (i_im, "unicorr") == PERFORM) {
	    if (FOC_IMG_MODE(caldat) == FOC_SPECTRO)
		call c_error (caldat, "SMMMODE = INBEAM but UNICORR = PERFORM")
	    DO_UNI(caldat) = true
	    UNI_FLAG(caldat) = PERFORM
	    UNI_LOG(caldat) = LOG_TRAILER + LOG_HEADER
	}

	if (cal_op_stat (i_im, "sdecorr") == PERFORM) {
	    if (FOC_IMG_MODE(caldat) == FOC_NORMAL)
		call c_error (caldat, "SMMMODE = NOTUSED but SDECORR = PERFORM")
	    DO_SDE(caldat) = true
	    SDE_FLAG(caldat) = PERFORM
	    SDE_LOG(caldat) = LOG_TRAILER + LOG_HEADER
	}

	if (DO_UNI(caldat)) {
	    # Get the value of UNIHFILE and check whether we need to call
	    # cal_g_uni in order to get the actual header file name.
	    call imgstr (i_im, "unihfile", unifile, SZ_PATHNAME)
	    if (streq (unifile, "N/A") || streq (unifile, "FNF"))
		EXPLICIT_UNI(caldat) = false
	    else
		EXPLICIT_UNI(caldat) = true	# UNI name given explicitly
	}

	# NOTE:  If we're going to call cal_g_uni, we have to call cal_g_phot
	# first in order to assign values to the PHOT array.

	if ((DO_ABS(caldat)) ||
	    (DO_UNI(caldat) && ( ! EXPLICIT_UNI(caldat) ))) {
	    # Get photometry information.
	    iferr (call cal_g_phot (i_im, phot, modestr,
				phot_path, SZ_PHOT_PATH))
		call xc_error (caldat)
	}

	# Check for the existence of reference files.

	if (DO_BAC(caldat)) {
	    call imgstr (i_im, "bachfile", tstring, SZ_PATHNAME)
	    if (imaccess (tstring, READ_ONLY) == NO) {
		files_not_found = files_not_found + 1
		call sprintf (text, SZ_PATHNAME, "BAC:  can't find %s")
		    call pargstr (tstring)
		call logmsg (text)
	    }
	}

	if (DO_ITF(caldat)) {
	    call imgstr (i_im, "itfhfile", tstring, SZ_PATHNAME)
	    if (imaccess (tstring, READ_ONLY) == NO) {
		files_not_found = files_not_found + 1
		call sprintf (text, SZ_PATHNAME, "ITF:  can't find %s")
		    call pargstr (tstring)
		call logmsg (text)
	    }
	}

	if (DO_GEO(caldat)) {
	    call imgstr (i_im, "geohfile", tstring, SZ_PATHNAME)
	    if (imaccess (tstring, READ_ONLY) == NO) {
		files_not_found = files_not_found + 1
		call sprintf (text, SZ_PATHNAME, "GEO:  can't find %s")
		    call pargstr (tstring)
		call logmsg (text)
	    }
	}

	if (DO_SDE(caldat)) {
	    call imgstr (i_im, "sdehfile", tstring, SZ_PATHNAME)
	    if (imaccess (tstring, READ_ONLY) == NO) {
		files_not_found = files_not_found + 1
		call sprintf (text, SZ_PATHNAME, "SDE:  can't find %s")
		    call pargstr (tstring)
		call logmsg (text)
	    }
	}

	if (DO_UNI(caldat)) {

	    # If we don't already have the name of the UNI file, we must
	    # get it from the UNITAB table.
	    if (! EXPLICIT_UNI(caldat) ) {
		# Get the relay name "F48" or "F96".
		call imgstr (i_im, "optcrly", tstring, SZ_PATHNAME)
		ip = 2		# skip the "F" and read the number (48 or 96)
		if (ctoi (tstring, ip, relay) < 1)
		    call c_error (caldat,
			    "the value of OPTCRLY is an invalid relay name")
		# Is the HRA deployed (f/288 mode)?
		call imgstr (i_im, "cammode", tstring, SZ_PATHNAME)

		# Get the name of the uni file.
		iferr (call cal_g_uni (i_im, phot[3], relay, tstring,
				unifile, SZ_PATHNAME))
		    call xc_error (caldat)
	    }
	    if (imaccess (unifile, READ_ONLY) == NO) {
		files_not_found = files_not_found + 1
		call sprintf (text, SZ_PATHNAME, "UNI:  can't find %s")
		    call pargstr (unifile)
		call logmsg (text)
	    }
	}

	if (files_not_found > 0) {
	    if (files_not_found > 1) {
		call sprintf (text, SZ_PATHNAME,
			"%d reference files could not be found.")
		    call pargi (files_not_found)
	    } else {
		call sprintf (text, SZ_PATHNAME,
			"One reference file could not be found.")
	    }
	    call c_error (caldat, text)
	}

	# BACCORR
	if (DO_BAC(caldat)) {
	    call imgstr( i_im, "bachfile", tstring, SZ_PATHNAME)
	    call bac_section (i_foc, section, SZ_FNAME)	# get image section
	    call strcat (section, tstring, SZ_PATHNAME)

	    iferr (BAC_FILE(caldat) = immap (tstring, READ_ONLY, 0)) {
		BAC_FILE(caldat) = NULL
		call xc_error (caldat)
	    }

            call sprintf( text, SZ_PATHNAME, "     BAC: %s")
                call pargstr( tstring)
            call logmsg( text )

	    call cal_check (BAC_FILE(caldat), "BAC",
			Memc[BAC_PEDIGREE(caldat)], Memc[BAC_DESCRIP(caldat)],
			SZ_PATHNAME)
	    if (strncmp (Memc[BAC_PEDIGREE(caldat)], "DUMMY", 5) == 0) {
		DO_BAC(caldat) = false
		BAC_FLAG(caldat) = SKIPPED
	    }
	}

	# ITFCORR - should be no need for image section unless the ITF
	# file is a dummy and is larger than the input image.
	if (DO_ITF(caldat)) {
	    call imgstr( i_im, "itfhfile", tstring, SZ_PATHNAME)

	    iferr (ITF_FILE(caldat) = immap( tstring, READ_ONLY, 0)) {
		ITF_FILE(caldat) = NULL
		call xc_error (caldat)
	    }

	    call sprintf( text, SZ_PATHNAME, "     ITF: %s")
		call pargstr( tstring)
	    call logmsg( text )

	    call cal_check (ITF_FILE(caldat), "ITF",
			Memc[ITF_PEDIGREE(caldat)], Memc[ITF_DESCRIP(caldat)],
			SZ_PATHNAME)
	    if (strncmp (Memc[ITF_PEDIGREE(caldat)], "DUMMY", 5) == 0) {
		DO_ITF(caldat) = false
		ITF_FLAG(caldat) = SKIPPED
	    } else if (IM_LEN (ITF_FILE(caldat), 1) > IM_LEN(i_im,1) ||
	        IM_LEN (ITF_FILE(caldat), 2) > IM_LEN(i_im,2)) {
		call logmsg ("Info:  ITF file is larger than needed")
	    }
	}

	# GEOCORR
	if (DO_GEO(caldat)) {
	    call imgstr( i_im, "geohfile", tstring, SZ_PATHNAME)

	    iferr (GEO_FILE(caldat) = immap( tstring, READ_ONLY, 0)) {
		GEO_FILE(caldat) = NULL
		call xc_error (caldat)
	    }

	    call sprintf( text, SZ_PATHNAME, "     GEO: %s")
		call pargstr( tstring)
	    call logmsg( text )

	    call cal_check (GEO_FILE(caldat), "GEO",
			Memc[GEO_PEDIGREE(caldat)], Memc[GEO_DESCRIP(caldat)],
			SZ_PATHNAME)
	    if (strncmp (Memc[GEO_PEDIGREE(caldat)], "DUMMY", 5) == 0) {
		DO_GEO(caldat) = false
		GEO_FLAG(caldat) = SKIPPED
	    }
	}

	# UNICORR
	if (DO_UNI(caldat)) {
	    # We got the UNIFILE name earlier; now get image section.
	    call uni_section (i_foc, caldat, section, SZ_FNAME)
	    call strcat (section, unifile, SZ_PATHNAME)

	    iferr (UNI_FILE(caldat) = immap (unifile, READ_ONLY, 0)) {
		UNI_FILE(caldat) = NULL
		call xc_error (caldat)
	    }

	    call sprintf (text, SZ_PATHNAME, "     UNI: %s")
		call pargstr (unifile)
	    call logmsg (text)

	    call cal_check (UNI_FILE(caldat), "UNI",
			Memc[UNI_PEDIGREE(caldat)], Memc[UNI_DESCRIP(caldat)],
			SZ_PATHNAME)
	    if (strncmp (Memc[UNI_PEDIGREE(caldat)], "DUMMY", 5) == 0) {
		DO_UNI(caldat) = false
		UNI_FLAG(caldat) = SKIPPED
	    }
	}

	# SDECORR
	if (DO_SDE(caldat)) {
	    # Get the name of the sde file, and get appropriate image section.
	    call imgstr (i_im, "sdehfile", tstring, SZ_PATHNAME)
	    call uni_section (i_foc, caldat, section, SZ_FNAME)
	    call strcat (section, tstring, SZ_PATHNAME)

	    iferr (SDE_FILE(caldat) = immap( tstring, READ_ONLY, 0)) {
		SDE_FILE(caldat) = NULL
		call xc_error (caldat)
	    }

	    call sprintf( text, SZ_PATHNAME, "     SDE: %s")
		call pargstr( tstring)
	    call logmsg( text )

	    call cal_check (SDE_FILE(caldat), "SPE",
			Memc[SDE_PEDIGREE(caldat)], Memc[SDE_DESCRIP(caldat)],
			SZ_PATHNAME)
	    if (strncmp (Memc[SDE_PEDIGREE(caldat)], "DUMMY", 5) == 0) {
		DO_SDE(caldat) = false
		SDE_FLAG(caldat) = SKIPPED
	    }
	}

	# Is there any calibration step to be done?
	DO_SOMETHING(caldat) =   (DO_BAC(caldat) || DO_ITF(caldat) ||
		DO_PXL(caldat) || DO_ABS(caldat) || DO_GEO(caldat) ||
		DO_UNI(caldat) || DO_SDE(caldat))

	DO_FLAT(caldat) = (DO_UNI(caldat) || DO_SDE(caldat))

	if (imaccf (i_im, "exptime") == YES)
	    EXP_TIME(caldat) = imgetr (i_im, "exptime")
	else
	    call c_error (caldat, "EXPTIME keyword not found")

	if (EXP_TIME(caldat) < 0.) {
	    EXP_TIME(caldat) = 0.
	    call logmsg ("EXPTIME is bad; zero will be used.")
	}

	# Allow the user to specify the background value for regions
	# of the output image that don't correspond to any portion of
	# the input image, due to the geometric correction.
	if (imaccf (i_im, "geodefv") == YES)
	    GEO_DEFV(caldat) = imgetr (i_im, "geodefv")
	else
	    GEO_DEFV(caldat) = 0.
end

# CLOS_CAL_FILES -- close the opened cal files
procedure clos_cal_files( caldat)

pointer	caldat		# i: cal data structure

begin
	if ( BAC_FILE(caldat) != NULL )
	    call imunmap( BAC_FILE(caldat) )
	if ( ITF_FILE(caldat) != NULL )
	    call imunmap( ITF_FILE(caldat) )
	if ( UNI_FILE(caldat) != NULL )
	    call imunmap( UNI_FILE(caldat) )
	if ( GEO_FILE(caldat) != NULL )
	    call imunmap( GEO_FILE(caldat) )
	if ( SDE_FILE(caldat) != NULL )
	    call imunmap( SDE_FILE(caldat) )
end

# CAL_OP_STAT -- check whether operation is omit, perform, complete or N/A

int procedure cal_op_stat( i_im, type) 

pointer	i_im			# i: image pointer
char	type[ARB]		# i: type of correction
#--
pointer sp
char	tvalue[SZ_FNAME]
int	nowhite(), junk, result
bool	streq()
pointer	msg
int	imaccf()

begin
	if (imaccf (i_im, type) == YES) {
	    call imgstr( i_im, type, tvalue, SZ_FNAME)
	} else {
	    call sprintf (tvalue, SZ_FNAME,
		"keyword %s not found, so that operation will be skipped")
		call pargstr (type)
	    call logmsg (tvalue)
	    return (OMIT)
	}

	junk = nowhite( tvalue, tvalue, SZ_FNAME)
	
	if ( streq( tvalue, "PERFORM") )
	    result = PERFORM
	else if ( streq( tvalue, "OMIT") )
	    result = OMIT
	else if ( streq( tvalue, "COMPLETE") )
	    result = COMPLETE
	else if ( streq( tvalue, "N/A") )
	    result = NOT_APPLICABLE
	else {
	    call smark (sp)
	    call salloc( msg, SZ_LINE, TY_CHAR)
	    call sprintf( Memc[msg], SZ_LINE, 
		"status for %s is >%s<; OMIT will be assumed")
		call pargstr( type)
		call pargstr( tvalue)
	    call logmsg (Memc[msg])
	    call sfree (sp)
	    return (OMIT)
	}
	return (result)
end

# CAL_CHECK -- perform simple checking on calibration file
# Compare the value of the FILETYPE keyword with CHECK to be sure they
# agree in the first N letters of FILETYPE, where N is the length of
# CHECK.  Get the PEDIGREE and DESCRIP keywords, if present.

procedure cal_check (im, check, pedigree, descrip, maxch)

pointer	im		# i: input image structure
char	check[ARB]	# i: character string to check
char	pedigree[ARB]	# o: pedigree string or EOS
char	descrip[ARB]	# o: descrip string or EOS
int	maxch		# i: maximum size of pedigree and descrip strings
#--
char	tstring[SZ_FNAME], keywd[SZ_FNAME]
int	strlen(), imaccf()
bool	strne()

begin
	# Check the FILETYPE keyword.
	if ( imaccf(im, "filetype") != YES ) {
	    call sprintf( tstring, SZ_FNAME,
			"%s does not have keyword FILETYPE")
		call pargstr( IM_NAME(im) )
	    call logmsg (tstring)
	} else {
	    call imgstr( im, "filetype", keywd, SZ_FNAME)
	    keywd[ strlen(check) + 1] = EOS
	    if (strne (keywd, check)) {
	        call sprintf( tstring, SZ_FNAME,
				"%s FILETYPE is %s, not %s")
		    call pargstr( IM_NAME(im) )
		    call pargstr( keywd )
		    call pargstr( check )
		call logmsg (tstring)
	    } 
	}

	# Get the values of PEDIGREE and DESCRIP, if they are present.
	if (imaccf (im, "pedigree") == YES)
	    call imgstr (im, "pedigree", pedigree, maxch)
	else
	    pedigree[1] = EOS
	if (imaccf (im, "descrip") == YES)
	    call imgstr (im, "descrip", descrip, maxch)
	else
	    descrip[1] = EOS
end

# CHAN_EXT -- append file extension

procedure chan_ext( infile, ext, newimage, maxch)

char	infile[ARB]	# i: input file name
char	ext[ARB]	# i: extension (including dot) to add
char	newimage[ARB]	# o: new file name
int	maxch		# i: max length of new name
#--
char	dot		# '.'
int	dotpos		# position of '.' in image name, or zero if no dot
int	strldx()	# last position of '.' in file name

begin
	# Copy infile to newimage and check for a dot in the name.
	call strcpy (infile, newimage, maxch)
	dot = '.'
	dotpos = strldx (dot, newimage)

	if (dotpos > 0)
	    newimage[dotpos] = EOS	# chop off old extension

	call strcat (ext, newimage, maxch)
end

# X_C_FOC_IMMAP -- open FOC image and get info about foc coordinates

define	SZ_PXFORMT	9	# long enough for NORMAL or ZOOM

pointer procedure x_c_foc_immap (image, foc_coord, accmode, foc_template)

char	image[SZ_FNAME]		# i: image name
pointer	foc_coord		# i: pointer to foc coord structure
int	accmode			# i: access mode
pointer	foc_template		# i: FOC structure template pointer
#--
pointer	im, template
char	pxformt[SZ_PXFORMT]	# "NORMAL" or "ZOOM"
int	sampbeg, linebeg
pointer	immap()
real	imgetr()
int	imaccf()
bool	streq()
errchk	immap

begin
	if (foc_template != NULL) {
	    if (accmode == NEW_COPY)
	 	template = FOC_IMPT(foc_template)
	    else 
		template = NULL
	} else
	    template = NULL

	im = immap (image, accmode, template)

	FOC_IMPT(foc_coord)	= im
	FOC_NPIX1(foc_coord)	= IM_LEN(im, 1)
	FOC_NPIX2(foc_coord)	= IM_LEN(im, 2)
	
	if (accmode == NEW_COPY) {
	    FOC_LINEOFF(foc_coord) = FOC_LINEOFF(foc_template)
	    FOC_SAMPOFF(foc_coord) = FOC_SAMPOFF(foc_template)
	    FOC_LINEBEG(foc_coord) = FOC_LINEBEG(foc_template)
	    FOC_SAMPBEG(foc_coord) = FOC_SAMPBEG(foc_template)
	    FOC_ZOOM_MODE(foc_coord) = FOC_ZOOM_MODE(foc_template) 
	} else {
	    if (imaccf (im, "SAMPOFF") == YES)
		FOC_SAMPOFF(foc_coord) = imgetr (im, "SAMPOFF")
	    else
		FOC_SAMPOFF(foc_coord) = 0.

	    if (imaccf (im, "LINEOFF") == YES)
		FOC_LINEOFF(foc_coord) = imgetr (im, "LINEOFF")
	    else
		FOC_LINEOFF(foc_coord) = 0.

	    sampbeg = nint (FOC_SAMPOFF(foc_coord)) + 1
	    linebeg = nint (FOC_LINEOFF(foc_coord)) + 1

	    if (imaccf (im, "PXFORMT") == YES) {

		call imgstr (im, "PXFORMT", pxformt, SZ_PXFORMT)
		call strupr (pxformt)
		FOC_ZOOM_MODE(foc_coord) = streq (pxformt, "ZOOM")

	    } else {			# assume not zoom format
		FOC_ZOOM_MODE(foc_coord) = false
	    }

	    # "Flip" sampbeg.  The value from the header refers to position
	    # on the photocathode, but we need the position of the first
	    # pixel of the image relative to the flipped photocathode.  This
	    # depends on whether the image is zoom or normal.
	    # NOTE that sampoff is not flipped!
	    if (FOC_ZOOM_MODE(foc_coord))
		sampbeg = 2 + MAX_NSAMPS - 2 * IM_LEN(im,1) - sampbeg
	    else
		sampbeg = 2 + MAX_NSAMPS - IM_LEN(im,1) - sampbeg

	    FOC_SAMPBEG(foc_coord) = sampbeg
	    FOC_LINEBEG(foc_coord) = linebeg
	}

	return (im)
end


# CAL_SPLIT -- dezoom the coordinate parameters
# If pixel splitting ("dezooming") is to be performed, the coordinate
# parameters should be modified accordingly.  CRPIX1, CD1_1, and CD2_1
# are gotten, changed, and replaced by this routine.

procedure cal_split (im)

pointer im		# i: pointer to imhdr structure
#--
real	crpix1		# reference pixel in x coordinate
real	cd1_1		# dl/dx
real	cd2_1		# dm/dx
real	imgetr()

begin
	ifnoerr (crpix1 = imgetr (im, "crpix1")) {
	    crpix1 = crpix1 * 2. - 0.5
	    call imputr (im, "crpix1", crpix1)
	}

	ifnoerr (cd1_1 = imgetr (im, "cd1_1")) {
	    cd1_1 = cd1_1 / 2.
	    call imputr (im, "cd1_1", cd1_1)
	}

	ifnoerr (cd2_1 = imgetr (im, "cd2_1")) {
	    cd2_1 = cd2_1 / 2.
	    call imputr (im, "cd2_1", cd2_1)
	}
end


# LOG_IMAGE -- print selected details about the image to log file
procedure log_image (i_foc)

pointer	i_foc			# i: input FOC image structure
#--
char	text[SZ_LINE]
int	sampbeg

begin

	call log_progress ("input images have been opened")

	sampbeg = nint (FOC_SAMPOFF(i_foc)) + 1

	call sprintf (text, SZ_LINE, 
		"        sampbeg: %d, linebeg: %d, naxis1: %d, naxis2: %d")
	    call pargi (sampbeg)		# "unflipped" sampbeg
	    call pargi (FOC_LINEBEG(i_foc))
	    call pargi (FOC_NPIX1(i_foc))
	    call pargi (FOC_NPIX2(i_foc))
	call logmsg (text)

	call sprintf (text, SZ_LINE, "         %s mode pixels")
	if (FOC_ZOOM_MODE(i_foc))
	    call pargstr ("ZOOM")
	else
	    call pargstr ("NORMAL")
	call logmsg (text)
end

# LOG_PROGRESS -- print time-stamped message
procedure log_progress( mess)

char	mess[ARB]
#--

long	clktime()
char	text[SZ_LINE]

begin

	call cnvtime (clktime(0), text, SZ_LINE)
	call strcat (" : ", text, SZ_LINE)
	call strcat (mess, text, SZ_LINE)
	call logmsg (text)

end

# xc_error -- get message & call error
# This routine is intended to be called after an error has been trapped, i.e.:
#	iferr (...)
#	    call xc_error()
# The purpose is to get the error number & message, prepend the rootname
# to the message, close all open images, and call error with the new message.
#
# Phil Hodge,  7-July-1989  subroutine created

procedure xc_error (caldat)

pointer caldat		# i: pointer to the calfoc struct
#--
pointer sp
pointer mess		# the error message
pointer fullmess	# the message preceded by rootname
int	istat		# the error code
int	errget()

char	rootname[SZ_FNAME]	# root name of the observation
common	/xcroot/ rootname	# root name, to be included in message

begin
	call smark (sp)
	call salloc (mess, SZ_LINE, TY_CHAR)
	call salloc (fullmess, SZ_LINE, TY_CHAR)

	istat = errget (Memc[mess], SZ_LINE)
	if (istat == OK) {
	    call sfree (sp)
	    return
	}

	# Reset the error condition so we can call error.
	call xer_reset()

	call clos_cal_files (caldat)

	call strcpy (rootname, Memc[fullmess], SZ_LINE)
	call strcat (" ", Memc[fullmess], SZ_LINE)
	call strcat (Memc[mess], Memc[fullmess], SZ_LINE)

	call error (istat, Memc[fullmess])
end

# c_error -- call error
# This routine closes all open images, prepends the root name of the
# observation to the input error message and then calls error.

procedure c_error (caldat, message)

pointer caldat		# i: pointer to the calfoc struct
char	message[ARB]	# i: error message
#--
pointer sp
pointer emess			# error message

char	rootname[SZ_FNAME]	# root name of the observation
common	/xcroot/ rootname	# root name, to be included in message

begin
	call smark (sp)
	call salloc (emess, SZ_PODPS_LINE, TY_CHAR)

	call clos_cal_files (caldat)

	call strcpy (rootname, Memc[emess], SZ_PODPS_LINE)
	call strcat (" ", Memc[emess], SZ_PODPS_LINE)
	call strcat (message, Memc[emess], SZ_PODPS_LINE)

	call error (1, Memc[emess])
end
