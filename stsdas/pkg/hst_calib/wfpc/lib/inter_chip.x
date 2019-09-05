#  inter_chip -- Perform the inter-chip correction
#
#  Description:
#  ------------
#  Perform the inter-chip correction by applying shifts, rotation, and scaling 
#  so CCD #1-4 are transformed to the coordinate of CCD #2 and CCD #5-8 to 
#  CCD #8 for WFPC.
#  
#  Date		Author			Description
#  ----		------			-----------
#  15-Oct-1992  J.-C. Hsu		adapted from Shawn Ewald's FORTRAN code
#------------------------------------------------------------------------------

procedure inter_chip (x0, y0, x, y, trans)

real	x0, y0		# input X and Y coordiantes 
real	x, y		# output X and Y in reference chip coordinate
real	trans[*]	# transformation coefficients

# trans[1] = 	shift_x
# trans[2] = 	shift_y
# trans[3] = 	scale
# trans[4] = 	scale_x
# trans[5] = 	scale_y
# trans[6] = 	sine of rot
# trans[7] = 	cosine of rot
#==============================================================================
begin

	# do rotation and shift
	x = trans[7] * x0 - trans[6] * y0 + trans[1]
	y = trans[6] * x0 + trans[7] * y0 + trans[2]

	# adjust for possible different scales between chips
	x = x * trans[3] / trans[4]
	y = y * trans[3] / trans[5]
end
