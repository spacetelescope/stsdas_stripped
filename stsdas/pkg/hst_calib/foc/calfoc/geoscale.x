include <imhdr.h>
include <math.h>

# Max allowed difference between the two position angles computed from the
# CD matrix.  If they differ by more than this, ORIENTAT will be used instead.
define	ANGLE_MAX_DIFF	0.1d0

# geo_scale -- assign values for the CD matrix
# This routine does one of two things, depending on SMMMODE.  Both involve
# setting values for the coordinate parameters.  If SMMMODE is INBEAM
# (spectrographic mode) in the GEO reference image header, the coordinate
# parameters from that file are copied to the output geometrically corrected
# image, replacing all coordinate parameters.  If SMMMODE is not INBEAM,
# only the values for the CD matrix of the geometrically corrected image
# are modified.  The orientation is taken from ORIENTAT (or from the CD
# matrix) from that image, and a value for the image scale is taken from
# the GEO correction file using keyword IMSCALE, which is in arcseconds per
# pixel.  Double precision is used in this routine, in contrast to geo_coord,
# which uses single precision.
#
# If IMSCALE is missing from the GEO correction image, this routine returns
# without making any change to the original CD matrix.  The orientation is
# gotten from ORIENTAT if that is present in the geometrically corrected
# image.  If not, the CD matrix is gotten.  If the CD matrix is present and
# non-singular, two position angles are computed from the CD matrix.  If the
# angles are nearly the same, then the average of those angles is used as
# the orientation; otherwise, this routine returns without changing anything.
#
# Phil Hodge,  7-Apr-1994  Subroutine created.

procedure geo_scale (g_im, geopt)

pointer g_im		# i: imhdr pointer for corrected image
pointer geopt		# i: imhdr pointer for GEO correction file
#--
pointer sp
pointer smmmode		# scratch for keyword value from GEO header
double	imscale		# image scale for both axes (degrees per pixel)
char	ctype1[SZ_CTYPE], ctype2[SZ_CTYPE]	# new values for ctype
double	crval[2]	# new values of crval
double	crpix[2]	# new values of crpix
double	ocd[2,2]	# old CD matrix
double	cd[2,2]		# new CD matrix
double	orientat	# position angle of Y axis, eastward from north
double	pa1, pa2	# position angles computed from input CD matrix
bool	specmode	# true if long-slit spectrographic mode
double	imgetd()
int	imaccf()
bool	streq(), strne()

begin
	call smark (sp)
	call salloc (smmmode, SZ_FNAME, TY_CHAR)

	# Get SMMMODE from GEO file.
	if (imaccf (geopt, "smmmode") == YES) {
	    call imgstr (geopt, "smmmode", Memc[smmmode], SZ_FNAME)
	    call strupr (Memc[smmmode])
	    specmode = streq (Memc[smmmode], "INBEAM")

	    # Get SMMMODE from geometrically corrected image for comparison.
	    if (imaccf (g_im, "smmmode") == YES) {
		call imgstr (g_im, "smmmode", Memc[smmmode], SZ_FNAME)
		call strupr (Memc[smmmode])
		if (strne (Memc[smmmode], "INBEAM") && specmode ||
		    streq (Memc[smmmode], "INBEAM") && !specmode) {
		    call logmsg (
		"Warning:  SMMMODE in input header and GEO header conflict.")
		}
	    }
	} else {
	    specmode = false
	}
	call sfree (sp)

	# For long-slit spectrographic mode, copy all coordinate parameters.
	if (specmode) {

	    # Get the coordinate parameters from the GEO file.
	    crval[1] = imgetd (geopt, "crval1")
	    crval[2] = imgetd (geopt, "crval2")
	    crpix[1] = imgetd (geopt, "crpix1")
	    crpix[2] = imgetd (geopt, "crpix2")
	    cd[1,1] = imgetd (geopt, "cd1_1")
	    cd[1,2] = imgetd (geopt, "cd1_2")
	    cd[2,1] = imgetd (geopt, "cd2_1")
	    cd[2,2] = imgetd (geopt, "cd2_2")
	    call imgstr (geopt, "ctype1", ctype1, SZ_CTYPE)
	    call imgstr (geopt, "ctype2", ctype2, SZ_CTYPE)

	    # Clobber old values in geometrically corrected image.
	    call imaddd (g_im, "crval1", crval[1])
	    call imaddd (g_im, "crval2", crval[2])
	    call imaddd (g_im, "crpix1", crpix[1])
	    call imaddd (g_im, "crpix2", crpix[2])
	    call imaddd (g_im, "cd1_1", cd[1,1])
	    call imaddd (g_im, "cd1_2", cd[1,2])
	    call imaddd (g_im, "cd2_1", cd[2,1])
	    call imaddd (g_im, "cd2_2", cd[2,2])
	    call imastr (g_im, "ctype1", ctype1)
	    call imastr (g_im, "ctype2", ctype2)

	    return
	}

	# We get here if the GEO file is for a normal image, not spectrographic.

	# Look for IMSCALE in the GEO reference file.
	if (imaccf (geopt, "imscale") == NO) {
	    call logmsg ("CD matrix not modified because IMSCALE is missing.")
	    return
	}

	# Get IMSCALE and convert from arcseconds / pixel to degrees / pixel.
	imscale = imgetd (geopt, "imscale") / 3600.d0

	if (imscale <= 0.d0) {
	    call logmsg (
		"CD matrix not modified because IMSCALE is not greater than 0.")
	    return
	}

	if (imaccf (g_im, "orientat") == YES) {

	    orientat = imgetd (g_im, "orientat")

	} else {

	    call logmsg ("ORIENTAT is missing ...")

	    # Get the original CD matrix to use for orientation.
	    if (imaccf (g_im, "cd1_1") == YES)
		ocd[1,1] = imgetd (g_im, "cd1_1")
	    else
		ocd[1,1] = 0.d0

	    if (imaccf (g_im, "cd1_2") == YES)
		ocd[1,2] = imgetd (g_im, "cd1_2")
	    else
		ocd[1,2] = 0.d0

	    if (imaccf (g_im, "cd2_1") == YES)
		ocd[2,1] = imgetd (g_im, "cd2_1")
	    else
		ocd[2,1] = 0.d0

	    if (imaccf (g_im, "cd2_2") == YES)
		ocd[2,2] = imgetd (g_im, "cd2_2")
	    else
		ocd[2,2] = 0.d0

	    # If ocd is singular, the CD matrix was either absent from the
	    # input image or it's garbage.
	    if (ocd[1,1] * ocd[2,2] - ocd[1,2] * ocd[2,1] == 0.d0) {
		call logmsg ("... input CD matrix is absent or singular ...")
		call logmsg ("... CD matrix not modified.")
		return
	    }

	    # Get the orientation from the CD matrix.  We compute two
	    # independent values and check that they are nearly equal.
	    pa1 = RADTODEG (atan2 (ocd[2,1], -ocd[1,1]))
	    pa2 = RADTODEG (atan2 (ocd[1,2], ocd[2,2]))

	    # atan2 returns a value between -pi and +pi.  If one value is
	    # just smaller than pi and the other just larger than -pi,
	    # add 2*pi to the negative one.
	    if (pa1 - pa2 < -180.d0)
		pa1 = pa1 + 360.d0
	    if (pa1 - pa2 > 180.d0)
		pa2 = pa2 + 360.d0

	    # If they don't satisfy this criterion, they differ too much.
	    if (abs (pa1 - pa2) > ANGLE_MAX_DIFF) {

		# We don't have enough information to compute the CD matrix.
		call logmsg (
		"... Input CD matrix gives two different position angles ...")
		call logmsg ("... CD matrix not modified.")

		return

	    } else {

		call logmsg ("... CD matrix used for orientation.")
		orientat = (pa1 + pa2) / 2.d0

	    }
	}

	cd[1,1] = -imscale * cos (DEGTORAD(orientat))
	cd[1,2] = imscale * sin (DEGTORAD(orientat))
	cd[2,1] = imscale * sin (DEGTORAD(orientat))
	cd[2,2] = imscale * cos (DEGTORAD(orientat))

	call imaddd (g_im, "cd1_1", cd[1,1])
	call imaddd (g_im, "cd1_2", cd[1,2])
	call imaddd (g_im, "cd2_1", cd[2,1])
	call imaddd (g_im, "cd2_2", cd[2,2])
end
