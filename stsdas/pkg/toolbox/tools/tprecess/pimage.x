include <imhdr.h>
include <math.h>
include <mach.h>
include "precess.h"
define	SZ_PNAME	8	# size of an image keyword name

# functional interface for precessing the coordinate system of an
# image.  Logically divided into two routine.  The test_image() verifies all
# necessary values are defined.  pimag_precess_image() performs/changes the 
# values of the image header cards.
#
# Andrew Cseko, Jr.      May-1990    Original
# Phil Hodge,  4-Jan-1993	Get EQUINOX keyword instead of EPOCH;
#				call stx_getcoord; include tpr_putcoord.
# Phil Hodge, 23-Mar-1994	In tpr_putcoord, don't try to reset MWCS.

# test_image -- determine if all necessary values are defined and correct.
# If something is amiss then a message describing the problem is returned
# in the parameter errmsg.

int procedure test_image (im, f_d_set, t_d_set, f_day, t_day,
	t_ctype1, errmsg, maxch)

pointer im                # i: image to operate on
bool    f_d_set, t_d_set  # i: were the f_year & t_year pars set by the user
double  f_day             # io: if not user set then set to value of EQUINOX
double  t_day             # i: user set precess-to year 
int     t_ctype1          # o: are axes 1-2 RA-DEC or DEC-RA
char    errmsg[ARB]       # o: on ERR message describing failure point
int     maxch             # i: size of string errmsg
#--
double	crpix[IM_MAXDIM]			# ignored
double	cd[IM_MAXDIM,IM_MAXDIM]			# ignored

char	equinox[SZ_FNAME]	# may include an initial letter, e.g. "J2000"
char    ctype[SZ_CTYPE,IM_MAXDIM]
double	crval[IM_MAXDIM]
double  new_ra, new_dec, old_ra, old_dec
double  zetaO, Z, theta

int	errno, errget()

int     imaccf(), strncmp()
int	ip, interpret_date()

begin
	# Get the coordinate parameters.  Here we only need crval & ctype.
	iferr {
	    call stx_getcoord (im, crpix, crval, cd, IM_MAXDIM, ctype, SZ_CTYPE)
	} then {
	    errno = errget (errmsg, maxch)
	    return (ERR)
	}
	crval[1] = DEGTORAD(crval[1])
	crval[2] = DEGTORAD(crval[2])

	# determine the "from" date, if it's not set

	if (!f_d_set) {
	    # check for EQUINOX card in header to use as from date

	    if (YES == imaccf (im, "EQUINOX")) {
		call imgstr (im, "EQUINOX", equinox, SZ_FNAME)
	    } else if (YES == imaccf (im, "EPOCH")) {
		call imgstr (im, "EPOCH", equinox, SZ_FNAME)
		call strcpy (
	"fromdate not set, and neither EQUINOX nor EPOCH found in image",
				errmsg, maxch)
		return (ERR)
	    }

	    ip = 1
	    if (interpret_date (equinox, ip, f_day) <= 0) {
		call strcpy ("could not interpret date from image header",
			errmsg, maxch)
		return (ERR)
	    }
	}

	if (!t_d_set) {
	    call strcpy ("todate not set", errmsg, maxch)
	    return (ERR)
	}

	# determine the precession angles
	call prec_angles (f_day, t_day, zetaO, Z, theta)

	#----------------------------------------------------------------
	# verify CTYPE cards are 'RA---TAN' etc, i.e. that precession applies

	# compare the first 2 or 3 characters to the strings "RA" or "DEC";
	# set the t_ctype1 variable respectively

	if (0 == strncmp (ctype[1,1], "RA", 2)) {
	    t_ctype1 = RA
	} else if (0 == strncmp (ctype[1,1], "DEC", 3)) {
	    t_ctype1 = DEC
	} else {
	    call sprintf (errmsg, maxch,
		    "CTYPE1 equals '%s' instead of 'RA' or 'DEC'")
		call pargstr (ctype[1,1])
	    return (ERR)
	}

	if (t_ctype1 == RA) {
	    if (0 != strncmp (ctype[1,2], "DEC", 3)) {
		call sprintf (errmsg, maxch,
			"CTYPE2 equals '%s' instead of 'DEC'")
		    call pargstr (ctype[1,2])
		return (ERR)
	    }
	} else {
	    if (0 != strncmp (ctype[1,2], "RA", 2)) {
		call sprintf (errmsg, maxch,
			"CTYPE2 equals '%s' instead of 'RA'")
		    call pargstr (ctype[1,2])
		return (ERR)
	    }
	}

	# check that the coordinate system of the image is precessable

	if ((strncmp (ctype[6,1], "AIT", 3) == 0) ||
	    (strncmp (ctype[6,2], "AIT", 3) == 0)) {
	    call strcpy ("may not precess Aitoff coordinates", errmsg, maxch)
	    return (ERR)
	}
	if ((strncmp (ctype[6,1], "MER", 3) == 0) ||
	    (strncmp (ctype[6,2], "MER", 3) == 0)) {
	    call strcpy ("may not precess Mercator coordinates", errmsg, maxch)
	    return (ERR)
	}

	#----------------------------------------------------------------
	# check that CRVAL for input and output are not at pole
	# > at this time the equations for precessing the CD matrix are
	# > not well defined when either the input or the output reference
	# > CRVALs are at the pole.  (A divide by zero error can result)

	# handle RA as either the X or Y axis of the image

	if (t_ctype1 == RA) {
	    old_ra = crval[1]
	    old_dec = crval[2]
	} else {
	    old_ra = crval[2]
	    old_dec = crval[1]
	}

	# compute precessed ra & dec of crval

	new_ra  = old_ra
	new_dec = old_dec

	call precess (new_ra, new_dec, zetaO, Z, theta)

	if (cos (old_dec) < 10*EPSILOND) {
	    call strcpy ("input declination at pole", errmsg, maxch)
	    return (ERR)
	}

	if (cos (new_dec) < 10*EPSILOND) {
	    call strcpy ("output declination at pole", errmsg, maxch)
	    return (ERR)
	}

	#----------------------------------------------------------------

	return (OK)
end

#----------------------------------------------------------------------------

# pimag_precess_image -- precess the coordinate system of an image.
# This routine was translated from the old SDAS precesion code. The
# file was FMATPREC.FOR.
#
# the following documentation was copied from the old code.
#
#  Compute cosine and sine of change in orientation due to precession
#  . See explanation below:
#  .
#  .        Theta   /!P1            The pole moves from P1 to P2;
#  .              /  !              Theta is a precession angle Theta;
#  .            /    !              fi is the angle we are looking for.
#  .          /      !
#  .        /!       !              S is the location of the reference
#  .  P2 ./  !       !              pixel with coordinates given by CRVAL
#  .      \b !       ! Pi/2-d1      (a1,d1), for the equator &
#  .       \ !       !              equinox with pole P1,
#  .        \!       !              (a2,d2) for the pole P2.
#  .         \       !
#  .          \      !              The point P1(the old pole) has right
#  . Pi/2-d2   \     !              ascension aP1 in the new coordinates
#  .            \----!              (i.e. relative to the new pole P2).
#  .             \ fi!
#  .              \  !              COS(Theta) = SIN(d1) * SIN(d2) +
#  .               \ !                         + COS(d1) * COS(d2) * COS(fi)
#  .                \!
#  .                 S      SIN(fi)/SIN(Theta) = SIN(b)/COS(d1) =
#  .                                           = SIN(aP1 - a2)  / COS(d1)
#  .               so:
#  .    COS(fi) = (COS(theta) - (SIN(d1)*SIN(d2))) / (COS(d1) * COS(d2))
#  .    SIN(fi) = (SIN(theta) * SIN(aP1 - a2)) / COS(d1
#  .
#   Rotate the old CD Matrix by the angle fi:
#
#		/                  \      /               \
#		| cos(fi)  -sin(fi) |     | CD1_1   CD1_2 |
#		|                   |  x  |               |
#		| sin(fi)   cos(fi) |     | CD2_1   CD2_2 |
#		\                   /      \              /
#
#        where CD1_1 = CDRA(1)   CD1_2 = CDRA(2)
#              CD2_1 = CDEC(1)   CD2_2 = CDEC(2)
#
# Andrew Cseko, Jr.         May 1990     Original

procedure pimag_precess_image (im, f_year, t_year, t_ctype1)

pointer im
double	f_year, t_year
int	t_ctype1	# from test_image : either == RA or DEC
#--
char	ctype[SZ_CTYPE,IM_MAXDIM]		# not changed
double	crpix[IM_MAXDIM]			# not changed

double	crval[IM_MAXDIM]
double	cd[IM_MAXDIM,IM_MAXDIM]
double	new_ra, new_dec, old_ra, old_dec
double	ra_1, ra_2, dec_1, dec_2
double	new_ra_1, new_ra_2, new_dec_1, new_dec_2
double	zetaO, Z, theta
double	pole_ra, pole_dec
double	cosphi, sinphi
double	jyear			# Julian year corresponding to t_year
char	prefix

double	jday_of()

begin
	prefix = ' '

	call prec_angles (jday_of(f_year,prefix), jday_of(t_year,prefix),
		zetaO, Z, theta)

	# get the values of the coordinate parameters
	call stx_getcoord (im, crpix, crval, cd, IM_MAXDIM, ctype, SZ_CTYPE)
	crval[1] = DEGTORAD(crval[1])
	crval[2] = DEGTORAD(crval[2])
	
	#----------------------------------------------------------------
	# handle RA as either the X or Y axis of the image

	if (t_ctype1 == RA) {
	    old_ra = crval[1]
	    old_dec = crval[2]
	    ra_1 =  cd[1,1]
	    ra_2 =  cd[1,2]
	    dec_1 = cd[2,1]
	    dec_2 = cd[2,2]
	} else {
	    old_ra = crval[2]
	    old_dec = crval[1]
	    ra_1 =  cd[2,1]
	    ra_2 =  cd[2,2]
	    dec_1 = cd[1,1]
	    dec_2 = cd[1,2]
	}
	
	# compute precessed ra & dec of crval

	new_ra  = old_ra
	new_dec = old_dec

	call precess (new_ra, new_dec, zetaO, Z, theta)

	#----------------------------------------------------------------
	# find the RA and DEC of the pole in the new/precessed coordinate system

	pole_ra =  DEGTORAD(0.d0)
	pole_dec = DEGTORAD(90.d0)
	call precess (pole_ra, pole_dec, zetaO, Z, theta)

	# determine the sin and cos components to rotation matrix

	cosphi =  (cos (theta) - sin (old_dec) * sin (new_dec)) /
	           (cos (old_dec) * cos (new_dec))
	sinphi = ABS (sin (theta)) * sin (new_ra - pole_ra) / cos (old_dec)

	# PHI = atan2 (sinphi, cosphi) 

	# rotate the CD matrix by phi

	new_ra_1 =  ra_1*cosphi - dec_1*sinphi
	new_ra_2 =  ra_2*cosphi - dec_2*sinphi
	new_dec_1 = ra_1*sinphi + dec_1*cosphi
	new_dec_2 = ra_2*sinphi + dec_2*cosphi

	#----------------------------------------------------------------
	# modify the coordinate parameters
	# handle RA as either the X or Y axis of the image

	if (t_ctype1 == RA) {
	    crval[1] = new_ra
	    crval[2] = new_dec
	    cd[1,1]  = new_ra_1
	    cd[1,2]  = new_ra_2
	    cd[2,1]  = new_dec_1
	    cd[2,2]  = new_dec_2
	} else {
	    crval[1] = new_dec
	    crval[2] = new_ra
	    cd[1,1]  = new_dec_1
	    cd[1,2]  = new_dec_2
	    cd[2,1]  = new_ra_1
	    cd[2,2]  = new_ra_2
	}

	crval[1] = RADTODEG(crval[1])
	crval[2] = RADTODEG(crval[2])
	call tpr_putcoord (im, crpix, crval, cd, IM_MAXDIM, ctype, SZ_CTYPE)

	# Convert t_year from Julian day number to Julian year, and add
	# the year to the output header.
	if (t_year > 2400000.d0)
	    jyear = (t_year - 2451545.d0) / 365.25d0 + 2000.d0
	else
	    jyear = t_year
	call imaddd (im, "EQUINOX", jyear)

	return
end

# tpr_putcoord -- assign coordinate parameters
# This routine adds coordinate parameters to the image header.

procedure tpr_putcoord (im, crpix, crval, cd, maxdim, ctype, maxch)

pointer im			# i: pointer to imhdr struct for output image
double	crpix[ARB]		# i: reference pixel
double	crval[ARB]		# i: coordinates at reference pixel
double	cd[maxdim,maxdim]	# i: derivatives of l & m with respect to x & y
int	maxdim			# i: size of arrays
char	ctype[maxch,maxdim]	# i: coord. type of first axis (e.g. "RA---TAN")
int	maxch			# i: size of ctype string
#--
char	keyword[SZ_PNAME]	# for keyword names
int	ndim
int	i, j

begin
	ndim = IM_NDIM(im)

	# Assign crpix, crval, ctype and the CD matrix in the output header.
	do i = 1, ndim {
	    call sprintf (keyword, SZ_PNAME, "crpix%d")
		call pargi (i)
	    call imaddd (im, keyword, crpix[i])
	    call sprintf (keyword, SZ_PNAME, "crval%d")
		call pargi (i)
	    call imaddd (im, keyword, crval[i])
	    call sprintf (keyword, SZ_PNAME, "ctype%d")
		call pargi (i)
	    call imastr (im, keyword, ctype[1,i])
	    do j = 1, ndim {
		call sprintf (keyword, SZ_PNAME, "cd%d_%d")
		    call pargi (i)
		    call pargi (j)
		call imaddd (im, keyword, cd[i,j])
	    }
	}
end
