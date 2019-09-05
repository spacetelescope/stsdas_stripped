include <imhdr.h>
include "calfoc.h"

# This file contains the following routines:
#	xhistory   put history info into output header regarding
#			calibration steps; also calls x_bac_log, etc.
#	x_bac_log  log info about BAC reference image to trailer
#	x_itf_log  log info about ITF reference image to trailer
#	x_geo_log  log info about GEO reference image to trailer
#	x_uni_log  log info about UNI reference image to trailer
#	x_sde_log  log info about SDE reference image to trailer
#	x_pxl_log  log info about dezooming to trailer
#	x_abs_log  log info about absolute DE to trailer

# xhistory -- put history info regarding reference files
# This routine adds history records to the output image header for each
# calibration step (that uses a reference image) that was either performed
# or skipped (as opposed to omitted).  The info gives the name of the
# reference (i.e. calibration) image and whether the step was performed or
# skipped.  If the reference image contained PEDIGREE and/or DESCRIP
# keywords, the values will be written as history records.  This routine
# also writes this info to STDOUT, which in the pipeline will be the
# trailer file.
#
# This routine may be called multiple times.  After info has been logged,
# the flag (e.g. GEO_LOG) will be reset, and on subsequent calls no further
# action will take place.
#
# Phil Hodge, 24-Mar-1994  Subroutines created.

procedure xhistory (oim, caldat)

pointer	oim			# i: imhdr pointer for output image
pointer	caldat			# i: cal files structure
#--
char	history[SZ_PATHNAME]	# history info

begin
	# These are the only two flags that get logged.
	if (BAC_FLAG(caldat) == COMPLETE || BAC_FLAG(caldat) == SKIPPED) {

	    # LOG_HEADER > LOG_TRAILER.  We may already have logged the
	    # info to the trailer file.
	    if (BAC_LOG(caldat) > LOG_TRAILER) {

		# Copy the calibration image name to the history record.
		call strcpy ("BACHFILE=", history, SZ_PATHNAME)
		call strcat (IM_HDRFILE(BAC_FILE(caldat)), history, SZ_PATHNAME)

		# Concatenate the calibration flag.  Also set the flag in the
		# header to say whether it was done or skipped.
		call strcat ("   BACCORR=", history, SZ_PATHNAME)
		if (BAC_FLAG(caldat) == COMPLETE) {
		    call strcat ("COMPLETE", history, SZ_PATHNAME)
		    call impstr (oim, "baccorr", "COMPLETE")
		} else if (BAC_FLAG(caldat) == SKIPPED) {
		    call strcat ("SKIPPED", history, SZ_PATHNAME)
		    call impstr (oim, "baccorr", "SKIPPED")
		}

		# Write the history record.
		call imputh (oim, "history", history)

		# Write PEDIGREE and DESCRIP as history, if they exist.
		if (Memc[BAC_PEDIGREE(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[BAC_PEDIGREE(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}
		if (Memc[BAC_DESCRIP(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[BAC_DESCRIP(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}

		# Subtracting LOG_HEADER leaves LOG_TRAILER; i.e. we still
		# need to log the info to the trailer file.
		BAC_LOG(caldat) = BAC_LOG(caldat) - LOG_HEADER
	    }

	    # Write the info to the trailer file.  We can't use the test
	    # whether BAC_FLAG = COMPLETE here (to see if we should log it
	    # at this time) because multiple calibration steps are performed
	    # in the same subroutine (xcalimg) that calls this routine.
	    call x_bac_log (caldat)
	}

	if (ITF_FLAG(caldat) == COMPLETE || ITF_FLAG(caldat) == SKIPPED) {

	    if (ITF_LOG(caldat) > LOG_TRAILER) {

		call strcpy ("ITFHFILE=", history, SZ_PATHNAME)
		call strcat (IM_HDRFILE(ITF_FILE(caldat)), history, SZ_PATHNAME)
		call strcat ("   ITFCORR=", history, SZ_PATHNAME)
		if (ITF_FLAG(caldat) == COMPLETE) {
		    call strcat ("COMPLETE", history, SZ_PATHNAME)
		    call impstr (oim, "itfcorr", "COMPLETE")
		} else if (ITF_FLAG(caldat) == SKIPPED) {
		    call strcat ("SKIPPED", history, SZ_PATHNAME)
		    call impstr (oim, "itfcorr", "SKIPPED")
		}
		call imputh (oim, "history", history)

		if (Memc[ITF_PEDIGREE(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[ITF_PEDIGREE(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}
		if (Memc[ITF_DESCRIP(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[ITF_DESCRIP(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}

		ITF_LOG(caldat) = ITF_LOG(caldat) - LOG_HEADER
	    }

	    call x_itf_log (caldat)
	}

	if (GEO_FLAG(caldat) == COMPLETE || GEO_FLAG(caldat) == SKIPPED) {

	    if (GEO_LOG(caldat) > LOG_TRAILER) {

		call strcpy ("GEOHFILE=", history, SZ_PATHNAME)
		call strcat (IM_HDRFILE(GEO_FILE(caldat)), history, SZ_PATHNAME)
		call strcat ("   GEOCORR=", history, SZ_PATHNAME)
		if (GEO_FLAG(caldat) == COMPLETE) {
		    call strcat ("COMPLETE", history, SZ_PATHNAME)
		    call impstr (oim, "geocorr", "COMPLETE")
		} else if (GEO_FLAG(caldat) == SKIPPED) {
		    call strcat ("SKIPPED", history, SZ_PATHNAME)
		    call impstr (oim, "geocorr", "SKIPPED")
		}
		call imputh (oim, "history", history)

		if (Memc[GEO_PEDIGREE(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[GEO_PEDIGREE(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}
		if (Memc[GEO_DESCRIP(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[GEO_DESCRIP(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}

		GEO_LOG(caldat) = GEO_LOG(caldat) - LOG_HEADER
	    }

	    # If it's SKIPPED, xcalimg will call x_geo_log separately.
	    if (GEO_FLAG(caldat) == COMPLETE)
		call x_geo_log (caldat)
	}

	if (UNI_FLAG(caldat) == COMPLETE || UNI_FLAG(caldat) == SKIPPED) {

	    if (UNI_LOG(caldat) > LOG_TRAILER) {

		# If we got the name of the uni file from unitab, save that
		# name in the output header.
		if ( ! EXPLICIT_UNI(caldat) )
		    call impstr (oim, "unihfile", IM_HDRFILE(UNI_FILE(caldat)))

		call strcpy ("UNIHFILE=", history, SZ_PATHNAME)
		call strcat (IM_HDRFILE(UNI_FILE(caldat)), history, SZ_PATHNAME)
		call strcat ("   UNICORR=", history, SZ_PATHNAME)
		if (UNI_FLAG(caldat) == COMPLETE) {
		    call strcat ("COMPLETE", history, SZ_PATHNAME)
		    call impstr (oim, "unicorr", "COMPLETE")
		} else if (UNI_FLAG(caldat) == SKIPPED) {
		    call strcat ("SKIPPED", history, SZ_PATHNAME)
		    call impstr (oim, "unicorr", "SKIPPED")
		}
		call imputh (oim, "history", history)

		if (Memc[UNI_PEDIGREE(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[UNI_PEDIGREE(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}
		if (Memc[UNI_DESCRIP(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[UNI_DESCRIP(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}

		UNI_LOG(caldat) = UNI_LOG(caldat) - LOG_HEADER
	    }

	    if (UNI_FLAG(caldat) == COMPLETE)
		call x_uni_log (caldat)
	}

	if (SDE_FLAG(caldat) == COMPLETE || SDE_FLAG(caldat) == SKIPPED) {

	    if (SDE_LOG(caldat) > LOG_TRAILER) {

		call strcpy ("SDEHFILE=", history, SZ_PATHNAME)
		call strcat (IM_HDRFILE(SDE_FILE(caldat)), history, SZ_PATHNAME)
		call strcat ("   SDECORR=", history, SZ_PATHNAME)
		if (SDE_FLAG(caldat) == COMPLETE) {
		    call strcat ("COMPLETE", history, SZ_PATHNAME)
		    call impstr (oim, "sdecorr", "COMPLETE")
		} else if (SDE_FLAG(caldat) == SKIPPED) {
		    call strcat ("SKIPPED", history, SZ_PATHNAME)
		    call impstr (oim, "sdecorr", "SKIPPED")
		}
		call imputh (oim, "history", history)

		if (Memc[SDE_PEDIGREE(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[SDE_PEDIGREE(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}
		if (Memc[SDE_DESCRIP(caldat)] != EOS) {
		    call strcpy ("  ", history, SZ_PATHNAME)
		    call strcat (Memc[SDE_DESCRIP(caldat)], history,
				SZ_PATHNAME)
		    call imputh (oim, "history", history)
		}

		SDE_LOG(caldat) = SDE_LOG(caldat) - LOG_HEADER
	    }

	    if (SDE_FLAG(caldat) == COMPLETE)
		call x_sde_log (caldat)
	}

	if (PXL_FLAG(caldat) == COMPLETE) {
	    call impstr (oim, "pxlcorr", "COMPLETE")
	    call x_pxl_log (caldat)
	}

	if (ABS_FLAG(caldat) == COMPLETE) {
	    call impstr (oim, "wavcorr", "COMPLETE")
	    call x_abs_log (caldat)
	}
end

# x_bac_log -- log info for BAC reference file
# This routine writes info to the trailer file (or STDOUT) regarding
# the BAC reference file name and status.  This is very similar to the
# corresponding section of xhistory.

procedure x_bac_log (caldat)

pointer	caldat			# i: cal files structure
#--
char	history[SZ_PATHNAME]	# history info

begin
	# LOG_HEADER = 2, and LOG_TRAILER = 1
	if (mod (BAC_LOG(caldat), LOG_HEADER) != LOG_TRAILER)
	    return				# we don't need to log it

	# Copy the calibration image name to the history record.
	call strcpy ("BACHFILE=", history, SZ_PATHNAME)
	call strcat (IM_HDRFILE(BAC_FILE(caldat)), history, SZ_PATHNAME)

	# Concatenate the calibration flag.
	call strcat ("   BACCORR=", history, SZ_PATHNAME)
	if (BAC_FLAG(caldat) == COMPLETE)
	    call strcat ("COMPLETE", history, SZ_PATHNAME)
	else if (BAC_FLAG(caldat) == SKIPPED)
	    call strcat ("SKIPPED", history, SZ_PATHNAME)

	# Write the info to the trailer file.
	call logmsg (history)

	# If PEDIGREE and DESCRIP exist, write them to trailer file.
	if (Memc[BAC_PEDIGREE(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[BAC_PEDIGREE(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}
	if (Memc[BAC_DESCRIP(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[BAC_DESCRIP(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}

	if (BAC_FLAG(caldat) == SKIPPED)
	    call logmsg (
		"  This step was skipped because the reference file is dummy.")

	# Subtracting LOG_TRAILER either leaves LOG_HEADER (i.e. we still
	# have to log to the header) or NO_LOG (i.e. nothing further to log).
	BAC_LOG(caldat) = BAC_LOG(caldat) - LOG_TRAILER
end

# x_itf_log -- log info for ITF reference file

procedure x_itf_log (caldat)

pointer	caldat			# i: cal files structure
#--
char	history[SZ_PATHNAME]	# history info

begin
	if (mod (ITF_LOG(caldat), LOG_HEADER) != LOG_TRAILER)
	    return

	call strcpy ("ITFHFILE=", history, SZ_PATHNAME)
	call strcat (IM_HDRFILE(ITF_FILE(caldat)), history, SZ_PATHNAME)

	call strcat ("   ITFCORR=", history, SZ_PATHNAME)
	if (ITF_FLAG(caldat) == COMPLETE)
	    call strcat ("COMPLETE", history, SZ_PATHNAME)
	else if (ITF_FLAG(caldat) == SKIPPED)
	    call strcat ("SKIPPED", history, SZ_PATHNAME)

	call logmsg (history)

	if (Memc[ITF_PEDIGREE(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[ITF_PEDIGREE(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}
	if (Memc[ITF_DESCRIP(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[ITF_DESCRIP(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}

	if (ITF_FLAG(caldat) == SKIPPED)
	    call logmsg (
		"  This step was skipped because the reference file is dummy.")

	ITF_LOG(caldat) = ITF_LOG(caldat) - LOG_TRAILER
end

# x_geo_log -- log info for GEO reference file

procedure x_geo_log (caldat)

pointer	caldat			# i: cal files structure
#--
char	history[SZ_PATHNAME]	# history info

begin
	if (mod (GEO_LOG(caldat), LOG_HEADER) != LOG_TRAILER)
	    return

	call strcpy ("GEOHFILE=", history, SZ_PATHNAME)
	call strcat (IM_HDRFILE(GEO_FILE(caldat)), history, SZ_PATHNAME)

	call strcat ("   GEOCORR=", history, SZ_PATHNAME)
	if (GEO_FLAG(caldat) == COMPLETE)
	    call strcat ("COMPLETE", history, SZ_PATHNAME)
	else if (GEO_FLAG(caldat) == SKIPPED)
	    call strcat ("SKIPPED", history, SZ_PATHNAME)

	call logmsg (history)

	if (Memc[GEO_PEDIGREE(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[GEO_PEDIGREE(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}
	if (Memc[GEO_DESCRIP(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[GEO_DESCRIP(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}

	if (GEO_FLAG(caldat) == SKIPPED)
	    call logmsg (
		"  This step was skipped because the reference file is dummy.")

	GEO_LOG(caldat) = GEO_LOG(caldat) - LOG_TRAILER
end

# x_uni_log -- log info for UNI reference file

procedure x_uni_log (caldat)

pointer	caldat			# i: cal files structure
#--
char	history[SZ_PATHNAME]	# history info

begin
	if (mod (UNI_LOG(caldat), LOG_HEADER) != LOG_TRAILER)
	    return

	call strcpy ("UNIHFILE=", history, SZ_PATHNAME)
	call strcat (IM_HDRFILE(UNI_FILE(caldat)), history, SZ_PATHNAME)

	call strcat ("   UNICORR=", history, SZ_PATHNAME)
	if (UNI_FLAG(caldat) == COMPLETE)
	    call strcat ("COMPLETE", history, SZ_PATHNAME)
	else if (UNI_FLAG(caldat) == SKIPPED)
	    call strcat ("SKIPPED", history, SZ_PATHNAME)

	call logmsg (history)

	if (Memc[UNI_PEDIGREE(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[UNI_PEDIGREE(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}
	if (Memc[UNI_DESCRIP(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[UNI_DESCRIP(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}

	if (UNI_FLAG(caldat) == SKIPPED)
	    call logmsg (
		"  This step was skipped because the reference file is dummy.")

	UNI_LOG(caldat) = UNI_LOG(caldat) - LOG_TRAILER
end

# x_sde_log -- log info for SDE reference file

procedure x_sde_log (caldat)

pointer	caldat			# i: cal files structure
#--
char	history[SZ_PATHNAME]	# history info

begin
	if (mod (SDE_LOG(caldat), LOG_HEADER) != LOG_TRAILER)
	    return

	call strcpy ("SDEHFILE=", history, SZ_PATHNAME)
	call strcat (IM_HDRFILE(SDE_FILE(caldat)), history, SZ_PATHNAME)

	call strcat ("   SDECORR=", history, SZ_PATHNAME)
	if (SDE_FLAG(caldat) == COMPLETE)
	    call strcat ("COMPLETE", history, SZ_PATHNAME)
	else if (SDE_FLAG(caldat) == SKIPPED)
	    call strcat ("SKIPPED", history, SZ_PATHNAME)

	call logmsg (history)

	if (Memc[SDE_PEDIGREE(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[SDE_PEDIGREE(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}
	if (Memc[SDE_DESCRIP(caldat)] != EOS) {
	    call strcpy ("  ", history, SZ_PATHNAME)
	    call strcat (Memc[SDE_DESCRIP(caldat)], history, SZ_PATHNAME)
	    call logmsg (history)
	}

	if (SDE_FLAG(caldat) == SKIPPED)
	    call logmsg (
		"  This step was skipped because the reference file is dummy.")

	SDE_LOG(caldat) = SDE_LOG(caldat) - LOG_TRAILER
end

# x_pxl_log -- log info for pixel dezooming

procedure x_pxl_log (caldat)

pointer	caldat			# i: cal files structure
#--

begin
	# We don't log this to the header since there's no reference image.
	if (PXL_LOG(caldat) != LOG_TRAILER)
	    return

	call logmsg ("PXLCORR:  Image has been dezoomed.")

	# We don't need to log this info anywhere else.
	PXL_LOG(caldat) = NO_LOG
end

# x_abs_log -- log info for absolute flux calibration

procedure x_abs_log (caldat)

pointer	caldat			# i: cal files structure
#--

begin
	# We don't log this to the header since there's no reference image.
	if (ABS_LOG(caldat) != LOG_TRAILER)
	    return

	call logmsg ("WAVCORR:  Photometry keywords have been computed.")

	# We don't need to log this info anywhere else.
	ABS_LOG(caldat) = NO_LOG
end
