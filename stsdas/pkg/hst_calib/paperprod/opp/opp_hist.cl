# plot the histogram for an STIS image

procedure opp_hist (input, igi_output, vleft, vright, vbottom, vtop, min, max)

char	input		{prompt = "input image file name"}
char	igi_output 	{prompt = "output igi script file name"}
real    vleft           {prompt = "left location of the plot"}
real    vright          {prompt = "right location of the plot"}
real    vbottom         {prompt = "bottom location of the plot"}
real    vtop            {prompt = "top location of the plot"}
real	min		{prompt = "lower limit of the data range"}
real	max		{prompt = "upper limit of the data range"}

begin
	# Declarations
	real	zmax, zmin		# max/min used in histogram
	char	im	
	char	script			# Igi script.
	char	tmp
	real	vl, vr, vb, vt		# location of the histogram
	real	margin
	real	size
	int	nbins
	
	# Get interactive parameters
	im = input
	script = igi_output

	zmin = min 
	zmax = max 
	nbins = 500
	size = 0.5

	# location
	vl = vleft
	vr = vright
	vb = vbottom
	vt = vtop
	margin = 0.05 * (vr - vl)

	# create the histogram text file
	tmp = mktemp ("tmp$PPOhist")
	imhistogram (im, z1=zmin, z2=zmax, binwidth = INDEF, nbins=nbins, 
			listout=yes, autoscale=no, top_closed=yes, > tmp)

	# Draw the acquisition images
	printf ("location %0.3f %0.3f %0.3f %0.3f\n", vl, vr, vb, vt, >> script)
	printf ("data %s\n", tmp, >> script)
	printf ("xcolumn 1\n", >> script)
	printf ("ycolumn 2\n", >> script)
	printf ("limits\n", >> script)
	printf ("margin 0.05\n", >> script)
	printf ("histogram\n", >> script)

	# draw a box around the histogram
	printf ("vmove %0.3f %0.3f\n", vl, vb, >> script)
	printf ("vdraw %0.3f %0.3f\n", vr, vb, >> script)
	printf ("vdraw %0.3f %0.3f\n", vr, vt, >> script)
	printf ("vdraw %0.3f %0.3f\n", vl, vt, >> script)
	printf ("vdraw %0.3f %0.3f\n", vl, vb, >> script)

	# draw labels
	printf ("fontset hard\n", >> script)
	printf ("expand %0.3f\n", size, >> script)
	printf ("justify 2\n", >> script)
	vb = vb - 0.01
	printf ("vmove %0.3f %0.3f\n", (vl+margin), vb, >> script)
	printf ("label %0.4g\n", zmin, >> script)
	printf ("vmove %0.3f %0.3f\n", (vr-margin), vb, >> script)
	printf ("label %0.4g\n", zmax, >> script)
end
