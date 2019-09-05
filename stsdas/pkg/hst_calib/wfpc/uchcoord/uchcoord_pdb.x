include <mach.h>
include "uchcoord.h"

#  uchcoord_pdb -- Populate aperture parameters copied from the Project
#		   Data Base (PDB).
#
#  Description:
#  ------------
#  
#  Date		Author		Description
#  ----		------		-----------
#  07-Feb-1996  J.-C. Hsu	Initial coding
#  22-Jun-2000  J.-C. Hsu	Add SIAF 13.0, 14.0 25.0
#  20-Mar-2001  J.-C. Hsu	implement real_change (vs. "better-knowledge")
#------------------------------------------------------------------------------
procedure uchcoord_pdb (v2, v3, scale, beta_y, xref, yref, epoch, epochs, 
			real_change, ep0)

real	v2[MAX_GROUP, ARB]
real	v3[MAX_GROUP, ARB]
real	scale[MAX_GROUP, ARB]
real	beta_y[MAX_GROUP, ARB]
real	xref[MAX_GROUP, ARB]
real	yref[MAX_GROUP, ARB]
double	epoch[ARB]
char	epochs[LEN_EPOCH, ARB]
bool	real_change[ARB]
int	ep0
#==============================================================================
begin

	ep0 = 6
	if (ep0 > MAX_EPOCH) call error (1, "too many epoches")

	# per Sylvia Baggett's request, change the epoch boundaries 
	# from Apr 5, 1994 to Apr 11, 1994, and from Feb 27, 1995 to Mar 27, 
	# 1995.
	epoch[1] = 49314.0	# MJD of Nov 23, 1993
	epoch[2] = 49453.0	# MJD of Apr 11, 1994
	epoch[3] = 49803.0	# MJD of Mar 27, 1995 (SIAF 11.0)

	# Add SIAF 13.0, 14.0, 25.0 6/22/2000, JCH
	epoch[4] = 50187.0	# MJD of Apr 14, 1996 (SIAF 13.0)
	epoch[5] = 50209.0	# MJD of May 06, 1996 (SIAF 14.0)
	epoch[6] = 50790.0	# MJD of Dec 08, 1997 (SIAF 25.0)

	# if real_change is true, they are real physical changes of the 
	# coordinate system (e.g. FGS realignment), other wise, they are
	# only "better-knowledge" changes.
	real_change[1] = false
	real_change[2] = false
	real_change[3] = false
	real_change[4] = true
	real_change[5] = false
	real_change[6] = true

	epoch[7] = MAX_REAL
	real_change[7] = true		# last one is always true

	call strcpy ("Nov 23, 1993", epochs[1, 1], LEN_EPOCH)
	call strcpy ("Apr 11, 1994", epochs[1, 2], LEN_EPOCH)
	call strcpy ("Mar 27, 1995", epochs[1, 3], LEN_EPOCH)
	call strcpy ("Apr 14, 1996", epochs[1, 4], LEN_EPOCH)
	call strcpy ("May 06, 1996", epochs[1, 5], LEN_EPOCH)
	call strcpy ("Dec 08, 1997", epochs[1, 6], LEN_EPOCH)

	# (1)
	# parameters installed on 23-Nov-93
	# V2 and V3 coordinates of the reference apertures, in arc seconds
	v2[1, 1] =   4.9464
	v2[2, 1] = -49.7989
	v2[3, 1] =   4.2287
	v2[4, 1] =  58.7918

	v3[1, 1] = -30.7741
	v3[2, 1] =  -5.6644
	v3[3, 1] =  48.0061
	v3[4, 1] =  -6.3428

	# plate scales, in arc seconds per pixel (use average if x and y have
	# different plate scales)
	scale[1, 1] = 0.046
	scale[2, 1] = 0.101
	scale[3, 1] = 0.101
	scale[4, 1] = 0.101

	# angles from V3 to the Y-axes, counterclockwise, on the sky,
	# in degrees. 
	beta_y[1, 1] = 225.
	beta_y[2, 1] = 315.
	beta_y[3, 1] =  45.
	beta_y[4, 1] = 135.

	# X and Y coordinates of the reference apertures, in pixels
	xref[1, 1] = 420.0
	xref[2, 1] = 423.5
	xref[3, 1] = 416.5
	xref[4, 1] = 423.0

	yref[1, 1] = 424.5
	yref[2, 1] = 414.0
	yref[3, 1] = 424.5
	yref[4, 1] = 421.0

	# (2)
	# parameters installed on 11-Apr-94
	# V2 and V3 coordinates of the reference apertures, in arc seconds
	v2[1, 2] =   1.87
	v2[2, 2] = -51.95
	v2[3, 2] =  -0.69
	v2[4, 2] =  54.4

	v3[1, 2] = -30.96
	v3[2, 2] =  -6.45
	v3[3, 2] =  48.06
	v3[4, 2] =  -6.5

	# plate scales, in arc seconds per pixel (use average if x and y have
	# different plate scales)
	scale[1, 2] = 0.0452
	scale[2, 2] = 0.0991
	scale[3, 2] = 0.0991
	scale[4, 2] = 0.0993

	# angles from V3 to the Y-axes, counterclockwise, on the sky,
	# in degrees. 
	beta_y[1, 2] = 224.72
	beta_y[2, 2] = 314.2
	beta_y[3, 2] =  44.54
	beta_y[4, 2] = 134.94

	# X and Y coordinates of the reference apertures, in pixels
	xref[1, 2] = 420.0
	xref[2, 2] = 423.5
	xref[3, 2] = 436.0
	xref[4, 2] = 423.0

	yref[1, 2] = 424.5
	yref[2, 2] = 414.0
	yref[3, 2] = 424.5
	yref[4, 2] = 421.0

	# (3)
	# parameters installed on 27-Mar-95
	# V2 and V3 coordinates of the reference apertures, in arc seconds
	v2[1, 3] =   1.8051
	v2[2, 3] = -51.9649
	v2[3, 3] =  -0.7149
	v2[4, 3] =  54.4351

	v3[1, 3] = -30.8958
	v3[2, 3] =  -6.4458
	v3[3, 3] =  48.0942
	v3[4, 3] =  -6.5758

	# plate scales, in arc seconds per pixel (use average if x and y have
	# different plate scales)
	scale[1, 3] = 0.04553
	scale[2, 3] = 0.09958
	scale[3, 3] = 0.09956
	scale[4, 3] = 0.09962

	# angles from V3 to the Y-axes, counterclockwise, on the sky,
	# in degrees. 
	beta_y[1, 3] = 224.69
	beta_y[2, 3] = 314.22
	beta_y[3, 3] =  44.52
	beta_y[4, 3] = 135.09

	# X and Y coordinates of the reference apertures, in pixels
	xref[1, 3] = 420.0
	xref[2, 3] = 423.5
	xref[3, 3] = 436.0
	xref[4, 3] = 423.0

	yref[1, 3] = 424.5
	yref[2, 3] = 414.0
	yref[3, 3] = 424.5
	yref[4, 3] = 421.0

	# (4)
	# parameters installed on 14-Apr-96
	# V2 and V3 coordinates of the reference apertures, in arc seconds
	v2[1, 4] =   1.5900
	v2[2, 4] = -52.1100
	v2[3, 4] =  -0.7300
	v2[4, 4] =  54.2500

	v3[1, 4] = -30.7700
	v3[2, 4] =  -6.2000
	v3[3, 4] =  48.2000
	v3[4, 4] =  -6.5900

	# plate scales, in arc seconds per pixel (use average if x and y have
	# different plate scales)
	scale[1, 4] = 0.045518
	scale[2, 4] = 0.099548
	scale[3, 4] = 0.099527
	scale[4, 4] = 0.099587

	# angles from V3 to the Y-axes, counterclockwise, on the sky,
	# in degrees. 
	beta_y[1, 4] = 224.89
	beta_y[2, 4] = 314.37
	beta_y[3, 4] =  44.68
	beta_y[4, 4] = 135.24

	# X and Y coordinates of the reference apertures, in pixels
	xref[1, 4] = 420.0
	xref[2, 4] = 423.5
	xref[3, 4] = 436.0
	xref[4, 4] = 423.0

	yref[1, 4] = 424.5
	yref[2, 4] = 414.0
	yref[3, 4] = 424.5
	yref[4, 4] = 421.0

	# (5)
	# parameters installed on 06-May-96
	# V2 and V3 coordinates of the reference apertures, in arc seconds
	v2[1, 5] =   2.1600
	v2[2, 5] = -51.5300
	v2[3, 5] =  -0.1500
	v2[4, 5] =  54.8300

	v3[1, 5] = -30.4900
	v3[2, 5] =  -5.9200
	v3[3, 5] =  48.4700
	v3[4, 5] =  -6.3200

	# plate scales, in arc seconds per pixel (use average if x and y have
	# different plate scales)
	scale[1, 5] = 0.045518
	scale[2, 5] = 0.099548
	scale[3, 5] = 0.099527
	scale[4, 5] = 0.099587

	# angles from V3 to the Y-axes, counterclockwise, on the sky,
	# in degrees. 
	beta_y[1, 5] = 224.89
	beta_y[2, 5] = 314.37
	beta_y[3, 5] =  44.68
	beta_y[4, 5] = 135.24

	# X and Y coordinates of the reference apertures, in pixels
	xref[1, 5] = 420.0
	xref[2, 5] = 423.5
	xref[3, 5] = 436.0
	xref[4, 5] = 423.0

	yref[1, 5] = 424.5
	yref[2, 5] = 414.0
	yref[3, 5] = 424.5
	yref[4, 5] = 421.0

	# (6)
	# parameters installed on 08-Dec-97
	# V2 and V3 coordinates of the reference apertures, in arc seconds
	v2[1, 6] =   2.3740
	v2[2, 6] = -51.3160
	v2[3, 6] =   0.0640
	v2[4, 6] =  55.0440

	v3[1, 6] = -30.2680
	v3[2, 6] =  -5.6980
	v3[3, 6] =  48.6920
	v3[4, 6] =  -6.0980

	# plate scales, in arc seconds per pixel (use average if x and y have
	# different plate scales)
	scale[1, 6] = 0.045518
	scale[2, 6] = 0.099548
	scale[3, 6] = 0.099527
	scale[4, 6] = 0.099587

	# angles from V3 to the Y-axes, counterclockwise, on the sky,
	# in degrees. 
	beta_y[1, 6] = 224.9080
	beta_y[2, 6] = 314.3880
	beta_y[3, 6] =  44.6980
	beta_y[4, 6] = 135.2580

	# X and Y coordinates of the reference apertures, in pixels
	xref[1, 6] = 420.0
	xref[2, 6] = 423.5
	xref[3, 6] = 436.0
	xref[4, 6] = 423.0

	yref[1, 6] = 424.5
	yref[2, 6] = 414.0
	yref[3, 6] = 424.5
	yref[4, 6] = 421.0
end
