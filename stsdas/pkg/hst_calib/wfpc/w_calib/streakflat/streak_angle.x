include <math.h>
include	"streakflat.h"

#  streak_angle -- calculate streak angle from the SHP header information
#
#  Description:
#  ------------
#  Use The following SHP header information to calculate the streak angle
#  (1) position angle of V3 axis, (2) RA and Dec of V1 axis, (3) space craft 
#  velocity.
#  
#  Header keywords needed:
#  ----------------------
#
#  "PA_V3"		Position angle of the V3 axis (PSANGLV3 in old files)
#  "RA_V1"		Right ascension of the V1 axis (RTASCNV1 in old files)
#  "DEC_V1"		Declination of the V1 axis (DECLNV1 in old files)
#  "VELOCSTX"		X-component of the space craft velocity
#  "VELOCSTY"		Y-component of the space craft velocity
#  "VELOCSTZ"		Z-component of the space craft velocity
#
#  Date		Author			Description
#  ----		------			-----------
#  26-May-1992  J.-C. Hsu		adapted from Jeff Hester's C code
#------------------------------------------------------------------------------

procedure streak_angle (fshp, strangle)

char	fshp[SZ_FNAME]
real	strangle

pointer	ip
real	pa_v3, ra_v1, dec_v1
real	vx, vy, vz
real	cos_ra, sin_ra, cos_dec, sin_dec
real	v_v2, v_v3

pointer	immap()
real	imgetr()
#==============================================================================
begin

	# open the SHP file 
	ip = immap (fshp, READ_ONLY, 0)

	# read keywords
	iferr (pa_v3 = imgetr (ip, "PA_V3"))
	    pa_v3 = imgetr (ip, "PSANGLV3")
	iferr (ra_v1 = imgetr (ip, "RA_V1"))
	    ra_v1 = imgetr (ip, "RTASCNV1")
	iferr (dec_v1 = imgetr (ip, "DEC_V1"))
	    dec_v1 = imgetr (ip, "DECLNV1")

	vx = imgetr (ip, "VELOCSTX")
	vy = imgetr (ip, "VELOCSTY")
	vz = imgetr (ip, "VELOCSTZ")

	cos_ra = cos(ra_v1/RADIAN)
	sin_ra = sin(ra_v1/RADIAN)
	cos_dec = cos(dec_v1/RADIAN)
	sin_dec = sin(dec_v1/RADIAN)

	# Let i, j, k represent the unit vectors of the coordinate system XYZ,
	# v1, v2, v3 the unit vectors of the ST vehicle cooordinate system
	# (corresponding to V1, V2, V3 axes with zero V3 position angle).
	# The vector [v1, v2, v3] can be obtained by rotating [i, j, k]
	# by ra around Z axis, and then dec around V2 axis, i. e. 
	#    /  \    /			     \ /		    \ / \
	#    |v1|    | cos(dec)  0   sin(dec)| |cos(ra)  sin(ra)  0 | |i|
	#    |v2| =  |   0      1      0     | |-sin(ra) cos(ra)  0 | |j|
	#    |v3|    | -sin(dec) 0   cos(dec)| |  0        0      1 | |k|
	#    \  /    \			     / \		    / \ /
	v_v3 = -cos_ra*sin_dec*vx - sin_ra*sin_dec*vy + cos_dec*vz
	v_v2 = -sin_ra*vx + cos_ra*vy 
	strangle = atan2(v_v2, v_v3)*RADIAN - pa_v3 + 90.

	call imunmap (ip)
end
