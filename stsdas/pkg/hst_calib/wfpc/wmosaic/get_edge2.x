#  get_edge2 -- Read the edge polynomials coefficients for WFPC2
#
#  Description:
#  ------------
#  Obtained from fitting a second order polynomial to K spots.  The K spots
#  positions were obtained from a 5-point centroiding, NOT corrected for 
#  geometric distortion.
#  
#  Date		Author			Description
#  ----		------			-----------
#  13-Jul-1994  J.-C. Hsu		Adapted from get_edge.x
#  23-Jun-1995  J.-C. Hsu		Add paddings to the seam
#------------------------------------------------------------------------------
procedure get_edge2 (ax, ay, iccd, order)

real	ax[*]
real	ay[*]
int	iccd
int	order

real	pad1, pad234
#==============================================================================
begin
	if (iccd >= 1 && iccd <= 4) order = 2

	# for x axis, the curve equation is y = ax[1] + ax[2]*x + ax[3]*x^2
	# for y axis, the curve equation is x = ay[1] + ay[2]*y + ay[3]*y^2

	# Add "padding" to the gap between K spots of different chips
	# This is (possibly) due to the centroiding bias of the K spots 
	# determination.  The shift is about 0.6 pixels, so it is 
	# 0.27(=0.6/2.187) WF pixel for PC1. (6/23/95 JCHsu)
	pad1 = 0.27
	pad234 = 0.6
	
        # From file g5.fit 
        if (iccd == 1) {
            ax[  1] = 52.47921 - pad1
            ax[  2] =  0.009072887
            ax[  3] = -9.941337e-6
            ay[  1] = 42.89779 - pad1
            ay[  2] =  0.009122855
            ay[  3] = -1.106709E-5
        }

        if (iccd == 2) {
            ax[  1] = 22.37283 - pad234
            ax[  2] =  0.01842164
            ax[  3] = -1.398300E-5
            ay[  1] = 48.28184 - pad234
            ay[  2] =  0.00265608
            ay[  3] = -1.468158E-5
        }

        if (iccd == 3) {
            ax[  1] = 44.78944 - pad234
            ax[  2] =  0.0138938
            ax[  3] = -1.412296E-5
            ay[  1] = 30.83626 - pad234
            ay[  2] =  0.008156041
            ay[  3] = -1.491324E-5
        }

        if (iccd == 4) {
            ax[  1] = 45.16632 - pad234
            ax[  2] =  0.003509023
            ax[  3] = -1.278723E-5
            ay[  1] = 41.51462 - pad234
            ay[  2] =  0.01273679
            ay[  3] = -1.063462E-5
        }
end
