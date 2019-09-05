include	<math.h>

#  get_trans2 -- Get the WFPC2 inter-chip transformation coefficients
#
#  Description:
#  ------------
#  Get the inter-chip correction coefficients of shifts, rotation, and scaling 
#  for the WFPC2 chips.  These corrections are relative to the second group 
#  See inter_chip.x for transforming algorithm.
#
#  Numbers are from Roberto Gilmozzi and Ellyne Kinney at STScI, Dec 1994.
#  
#  Date		Author			Description
#  ----		------			-----------
#  05-Jul-1994  J.-C. Hsu		adapted from get_trans.x
#------------------------------------------------------------------------------

procedure get_trans2 (iccd, trans)

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
	    shift_x = 58.676
	    shift_y = 101.347
	    scale = 0.4573045
	    scale_x = 1.
	    scale_y = 1.0002793
	    rot = 270.496
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
	    shift_x = 98.627
	    shift_y = -6.509
	    scale = 0.9994667
	    scale_x = 1.
	    scale_y = 1.
	    rot = 90.327
	} 
	if (iccd == 4) {
	    shift_x = 95.430
	    shift_y = 75.480
	    scale = 0.9999972
	    scale_x = 1.
	    scale_y = 1.
	    rot = 180.899
	} 
	    trans[1] = shift_x
	    trans[2] = shift_y
	    trans[3] = scale
	    trans[4] = scale_x
	    trans[5] = scale_y
	    trans[6] = sin (rot/RADIAN)
	    trans[7] = cos (rot/RADIAN)
end
