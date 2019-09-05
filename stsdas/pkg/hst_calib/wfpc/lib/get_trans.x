include	<math.h>

#  get_trans -- Get the inter-chip transformation coefficients
#
#  Description:
#  ------------
#  Get the inter-chip correction coefficients of shifts, rotation, and scaling 
#  for the WFPC chips.  These corrections are relative to the second group 
#  (i.e. chip #2 for WF and chip #6 for PC).  See inter_chip.x for transforming
#  algorithm.
#
#  Numbers are from Roberto Gilmozzi and Shawn Ewald at STScI, Oct 1992.
#  
#  Date		Author			Description
#  ----		------			-----------
#  15-Oct-1992  J.-C. Hsu		adapted from Shawn Ewald's FORTRAN code
#------------------------------------------------------------------------------

procedure get_trans (iccd, trans)

int	iccd
real	trans[*]

real	rot		# rotation angle relative to the reference chip
real	shift_x
real	shift_y
real	scale
real	scale_x
real	scale_y

# trans[1] = 	shift_x
# trans[2] = 	shift_y
# trans[3] = 	scale
# trans[4] = 	scale_x
# trans[5] = 	scale_y
# trans[6] = 	sine of rot
# trans[7] = 	cosine of rot
#==============================================================================
begin

	if (iccd == 1) {
	    shift_x = -14.0532
	    shift_y =  55.8813
	    scale = 1.000046
	    scale_x = 0.998332
	    scale_y = 0.998726
	    rot = 269.44883
	} 
	if (iccd == 2) {
	    shift_x = 0.
	    shift_y = 0.
	    scale = 1.
	    scale_x = 1.
	    scale_y = 1.
	    rot = 0.
	} 
	if (iccd == 3) {
	    shift_x = 45.262
	    shift_y =  4.933
	    scale = 1.000005
	    scale_x = 1.
	    scale_y = 1.
	    rot = 90.18741
	} 
	if (iccd == 4) {
	    shift_x = 36.584
	    shift_y = 61.12
	    scale = 1.000014
	    scale_x = 1.
	    scale_y = 0.9996
	    rot = 180.30538
	} 
	if (iccd == 5) {
	    shift_x = -0.15
	    shift_y = 71.36
	    scale = 1.0026
	    scale_x = 1.
	    scale_y = 1.
	    rot = 269.8153
	} 
	if (iccd == 6) {
	    shift_x = 0.
	    shift_y = 0.
	    scale = 1.
	    scale_x = 1.
	    scale_y = 1.
	    rot = 0.
	} 
	if (iccd == 7) {
	    shift_x = 63.75
	    shift_y = 3.7
	    scale = 0.99935
	    scale_x = 1.
	    scale_y = 1.
	    rot = 90.7527
	} 
	if (iccd == 8) {
	    shift_x = 51.61
	    shift_y = 64.58
	    scale = 1.
	    scale_x = 1.
	    scale_y = 1.
	    rot = 180.407
	} 
	    trans[1] = shift_x
	    trans[2] = shift_y
	    trans[3] = scale
	    trans[4] = scale_x
	    trans[5] = scale_y
	    trans[6] = sin (rot/RADIAN)
	    trans[7] = cos (rot/RADIAN)
end
