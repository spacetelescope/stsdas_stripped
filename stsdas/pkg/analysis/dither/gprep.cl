# GPREP  - Convert drizzle parameters for one WFPC2 group
#          into the appropriate ones for the other chips to form
#          a mosaic.
#
# Test version with chip 3 as reference
#
#  14-Aug-98: first attempt             (Richard Hook & Andy Fruchter)
#  28-Sep-98: new chip offsets from Stefano Casertano
#  02-Oct-98: correct rotation bug, tidy up. (Richard Hook)
#  09-Nov-98: add extra rotation in output (Richard Hook)
#  12-Nov-98: correct chip 4 rotation angle bug (Richard Hook)
#  29-Jun-00: Specify chip offset parameters in a pset   (Anton Koekemoer)

procedure gprep (image, axsh, aysh, arot)

char	image	= ""    {prompt="Input image file name from avshift"}
real	axsh	= 0.0	{prompt="Best chip 3 X shift from avshift"}
real	aysh	= 0.0	{prompt="Best chip 3 Y shift from avshift"}
real	arot	= 0.0	{prompt="Rotation from avshift"}
real	xsh	= 0.0	{prompt="X shift for reference chip 3"}
real	ysh	= 0.0	{prompt="Y shift for reference chip 3"}
real	rot	= 0.0	{prompt="Rotation angle (degrees anticlockwise)"}
real	scale   = 1.0	{prompt="Scale factor"}
pset	wfpc2_chips     {prompt="WFPC2 chip offset parameters"}

begin
	# Offsets and angles relative to other chips
	int	jj
	real	rad[4]
	real	t1, t2, t4
	real	xs[4], ys[4], r[4], s[4]
	real	xd[4], yd[4]
      	real 	rr, axx, ayy
	real	ax, ay, ar	
	file	im

	# Get chip 3 values from input
        xs[3] = xsh
        ys[3] = ysh
        r[3] = rot
        s[3] = scale

        im=image

	# Get AVSHIFT addition shift and rotation
	ax = axsh
	ay = aysh
	ar = arot

        # New parameters from Stefano Casertano's new geometric fit
	# These are for his "modified trauger" solution.
	# 29th September 1998

        # Set standard values, relative to chip 3
        # These are the angular positions of the chips relative to the
        # axes of chip3 (degrees)
        t1= 3.1415926/180.0 * wfpc2_chips.t_1
        t2= 3.1415926/180.0 * wfpc2_chips.t_2
        t4= 3.1415926/180.0 * wfpc2_chips.t_4

        # Radial distances in WF3 pixels from centre of WF3 to centres
 	# of other chips (401,401).
        rad[1] = wfpc2_chips.rad_1
        rad[2] = wfpc2_chips.rad_2
        rad[3] = wfpc2_chips.rad_3
        rad[4] = wfpc2_chips.rad_4

	# Set the shifts to match the one specified for chip 3
        xd[1] = xs[3] 
        yd[1] = ys[3] 
        xd[2] = xs[3] 
        yd[2] = ys[3] 
        xd[3] = xs[3] 
        yd[3] = ys[3] 
        xd[4] = xs[3] 
        yd[4] = ys[3] 

	# Add rotations
	r[3] = r[3]+ar
	r[1] = r[3]+wfpc2_chips.r_1
	r[2] = r[3]+wfpc2_chips.r_2
	r[4] = r[3]+wfpc2_chips.r_4

	# Scale appropriately
	s[1] = s[3] * wfpc2_chips.s_3 / wfpc2_chips.s_1
	s[2] = s[3] * wfpc2_chips.s_3 / wfpc2_chips.s_2
	s[4] = s[3] * wfpc2_chips.s_3 / wfpc2_chips.s_4

	# Add up the two components of the shifts
	rr = r[3]*3.1415926/180.0
	ar = ar*3.1415926/180.0

        # First the rotated version of the AVSHIFTS
        axx = ax * cos(rr) - ay * sin(rr)
        ayy = ax * sin(rr) + ay * cos(rr)

	xs[1] = xd[1] + (axx-rad[1] * cos(t1+rr))/s[3]
	xs[2] = xd[2] + (axx-rad[2] * cos(t2+rr))/s[3]
	xs[3] = xd[3] + axx/s[3]
	xs[4] = xd[4] + (axx-rad[4] * cos(t4+rr))/s[3]

	ys[1] = yd[1] + (ayy-rad[1] * sin(t1+rr))/s[3]
	ys[2] = yd[2] + (ayy-rad[2] * sin(t2+rr))/s[3]
	ys[3] = yd[3] + ayy/s[3]
	ys[4] = yd[4] + (ayy-rad[4] * sin(t4+rr))/s[3]

	# Write out the results
        printf("Image         Group  xsh      ysh       rot       scale\n")
        for (jj=1; jj<=4; jj+=1) {
	   printf("%s %1d %8.3f  %8.3f  %8.3f %8.4f\n", im,jj,xs[jj],ys[jj],r[jj],s[jj])
        }

end
