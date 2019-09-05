#  get_edge -- Read the edge polynomials coefficients
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  15-Mar-1993  J.-C. Hsu		coding
#------------------------------------------------------------------------------
procedure get_edge (ax, ay, iccd, order)

real	ax[*]
real	ay[*]
int	iccd
int	order
#==============================================================================
begin
	if (iccd >= 1 && iccd <= 4) order = 2
	if (iccd >= 5 && iccd <= 8) order = 2

	# for x axis, the curve equation is y = ax[1] + ax[2]*x + ax[3]*x^2
	# for y axis, the curve equation is x = ay[1] + ay[2]*y + ay[3]*y^2

        # From file u01.fit 
        if (iccd == 1) {
            ax[  1] = 29.04028
            ax[  2] =  0.02021268
            ax[  3] =  -1.179693e-5
            ay[  1] = 27.53231
            ay[  2] = -0.002759853
            ay[  3] = -1.062834e-5
        }

        if (iccd == 2) {
            ax[  1] =  25.3527
            ax[  2] =  0.01079901
            ax[  3] =  -1.218734e-5
            ay[  1] =  14.07325
            ay[  2] =  0.009556286
            ay[  3] = -1.316108e-5
        }

        if (iccd == 3) {
            ax[  1] = 28.35366
            ax[  2] = 0.007893964
            ax[  3] =  -1.38048e-5
            ay[  1] =  21.72509
            ay[  2] = 0.01303165
            ay[  3] = -1.337317e-5
        }

        if (iccd == 4) {
            ax[  1] = 31.40168
            ax[  2] =  0.005492746
            ax[  3] = -1.246375e-5
            ay[  1] =  18.227
            ay[  2] =  0.01385086
            ay[  3] =  -1.333298e-5
        }

        # From file t01.fit
        if (iccd == 5) {
            ax[  1] =  28.5738
            ax[  2] =  0.02323923
            ax[  3] =  -1.147424e-5
            ay[  1] =  42.05098
            ay[  2] =  -0.004069912
            ay[  3] =  -1.282343e-5
        }

        if (iccd == 6) {
            ax[  1] =  24.98943
            ax[  2] =  0.01937818
            ax[  3] =  -1.172252e-5
            ay[  1] =  26.77648
            ay[  2] =  0.001472063
            ay[  3] =  -1.444498e-5
        }

        if (iccd == 7) {
            ax[  1] =  32.29063
            ax[  2] =   0.004164584
            ax[  3] =  -1.039446e-5
            ay[  1] =  23.25528
            ay[  2] =  0.01436684
            ay[  3] =  -1.339986e-5
        }

        if (iccd == 8) {
            ax[  1] =  32.90641
            ax[  2] =  0.01233545
            ax[  3] = -1.210588e-5
            ay[  1] =  18.15718
            ay[  2] =  0.008472322
            ay[  3] =  -1.480377e-5
        }
end
