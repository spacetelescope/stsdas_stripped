procedure ypeakup (root)

char	root	{prompt="Rootname of peakup observation"}
bool	plot	{yes,prompt="Produce plot?"}
bool	v2v3off	{no,prompt="Produce only V2/V3 offsets?"}
char	pos_tab	{"",prompt="Name of the position table"}
bool	short_title {no,prompt="Place short title on graph?"}
char	igi_script {"",prompt="Name of igi script file to create"}
char	device	{"stdgraph",prompt="Graphics device"}
real	xoff	{prompt="Output: X offset"}
real	yoff	{prompt="Output: Y offset"}
real	v2off	{prompt="Output: V2 offset"}
real	v3off	{prompt="Output: V3 offset"}

struct	*list
struct	*list2

begin
	# Delcarations.
	char	aper_fov
	char	cdfile
	bool	costar
	char	d0h
	char	det
	int	dpl
	real	ds
	int	i, j, k
	int	lps
	real	ls
	int	mdwell
	char	merge
	int	new
	int	np
	real	pa
	char	pos
	char	proot
	int	rho
	char	script
	char	shp
	int	sign
	real	sl
	char	sum, sum2
	real	sw
	char	sx
	char	tmproot
	char	uparfile
	real	x, xhigh, xlow, xpos
	real	y, yhigh, ylow, ypos
	
	# Get interactive parameters
	proot = root

	# Temporary files.
	tmproot = mktemp ("tmp$YPEAKUP")
	cdfile = tmproot//"_cd.txt"
	merge = tmproot//"_merge.txt"
	pos = tmproot//"_pos.txt"
	if (igi_script == "")
	    script = tmproot//"_script.igi"
	else
	    script = igi_script
	sum = tmproot//"_sum.txt"
	sum2 = tmproot//"_sum2.txt"
	uparfile = tmproot//"_upar.txt"

	# Set other file names.
	d0h = proot//".d0h"
	shp = proot//".shh"

	# Get relevant keywords.
	keypar (d0h, "aper_fov", silent=yes)
	aper_fov = keypar.value
	
	# Create a table with the sum of the counts for each group
	# of the peakup.
	gstatistics (d0h, masks="", groups="*", g_accum=no,
		     fields="sum", lower=INDEF, upper=INDEF,
		     gstpar="", > sum2)

	# for single group images, gstatistics only output one ccolumn 
	# rewrite the table "sum2" for this case (JC Hsu 7/18/1996)
	tinfo (sum2, > "dev$null")
	if (tinfo.nrows == 1) {
	    if (tinfo.ncols == 1) {
		delete (sum2)
		print ("1 ", gstpar.sum, > sum2)
	    } else {
		print (sum2, " has abnormal format")
	    }
	}

	tprint (sum2, prparam=no, prdata=yes, pwidth=80, plength=0,
		showrow=no, showhdr=no, showunits=no, columns="",
		rows="-", option="plain", align=yes, sp_col="", lgroup=0,
		> sum)
	tinfo (sum, ttout=no)
	np = tinfo.nrows

	# List out the sums.
	if (!v2v3off) {
	    printf ("%d dwell points in aperture %s:\n", np, aper_fov)
	    tprint (sum, prparam=no, prdata=yes, pwidth=80, plength=0,
		    showrow=no, showhdr=no, showunits=no, columns="c2",
		    rows="-", option="plain", align=yes, sp_col="", lgroup=0) |
		    table ("", first_col=0, last_col=0, ncols=0, maxstrlen=0)
	}

	# Get spatial scan info.
	keypar (shp, "dwell_ln", silent=yes)
	dpl = int (keypar.value)
	keypar (shp, "no_lines", silent=yes)
	lps = int (keypar.value)
	keypar (shp, "scan_len", silent=yes)
	sl = real (keypar.value)
	keypar (shp, "scan_wid", silent=yes)
	sw = real (keypar.value)
	keypar (shp, "scan_ang", silent=yes)
	pa = real (keypar.value)
	keypar (shp, "ss_det", silent=yes)
	det = keypar.value

	# Look in the SHP data for moving target information.
	listpix (shp//"[3][880:880]", wcs="logical",
		 formats="", verbose=no) | scan (i, new)

	# Convert position angle so it's from Y.
	if (det == "AMBER")
	    x = 81.81 + pa
	else
	    x = 8.19 + pa
	rho = int (x + 0.5)
	rho = rho % 360
	if (new == 0 && np == 3) {
	    print ("Moving Target Peakup.")
	    rho = 0
	}

	# Calculate dwell and line spacing.
	ds = sl / real (dpl - 1)
	if (lps > 1)
	    ls = sw / real (lps - 1)
	else
	    ls = 0

	# Now calculate the x,y positions for each scan format
	switch (rho) {
	case 0:
	    {
		ypos = -sl / 2.
		xpos = -sw / 2.
		sign = 1
		for (y=1; y <= lps; y=y+1) {
		    for (x=1; x <= dpl; x=x+1) {
			print (xpos, ypos, >> pos)
			ypos = ypos + (sign * ds)
		    }
		    sign = -sign
		    ypos = ypos + (sign * ds)
		    xpos = xpos + ls
		}
	    }

	case 90:
	    {
		xpos = -sl / 2.
		ypos = -sw / 2.
		sign = 1
		for (y=1; y <= lps; y=y+1) {
		    for (x=1; x <= dpl; x=x+1) {
			print (xpos, ypos, >> pos)
			xpos = xpos + (sign * ds)
		    }
		    sign = -sign
		    xpos = xpos + (sign * ds)
		    ypos = ypos + ls
		}
	    }

	case 180:
	    {
		ypos = sl / 2.
		xpos = sw / 2.
		sign = -1
		for (y=1; y <= lps; y=y+1) {
		    for (x=1; x <= dpl; x=x+1) {
			print (xpos, ypos, >> pos)
			ypos = ypos + (sign * ds)
		    }
		    sign = -sign
		    ypos = ypos + (sign * ds)
		    xpos = xpos - ls
		}
	    }

	case 270:
	    {
		xpos = sl / 2.
		ypos = sw / 2.
		sign = -1
		for (y=1; y <= lps; y=y+1) {
		    for (x=1; x <= dpl; x=x+1) {
			print (xpos, ypos, >> pos)
			xpos = xpos + (sign * ds)
		    }
		    sign = -sign
		    xpos = xpos + (sign * ds)
		    ypos = ypos - ls
		}
	    }

	default:
	    printf ("Not supported: rho = %d\n", rho)
	}

	# Find dwell with the maximum counts.
	list = sum
	i = fscan (list, mdwell, x)
	while (fscan (list, i, y) != EOF)
	    if (y > x) {
		mdwell = i
		x = y
	    }

	# Find V2/V3 offset for the maximum dwell.
	list=pos
	for (i=1; i<=mdwell; i=i+1)
	    j = fscan (list, xoff, yoff)

	# Note: the below, apparently redundant, assignment seems
	# necessary because, if not done, the values for these
	# parameters will not appear when one accesses them from
	# outside this script.  Bug in the cl?
	xoff = xoff
	yoff = yoff
	
	keypar (d0h, "expstart", silent=yes)
	costar = (int (keypar.value) >= 49348)
	yv2v3_calculate (xoff, yoff, det, costar=costar)
	v2off = yv2v3_calculate.v2
	v3off = yv2v3_calculate.v3

	# Some extra output.
	if (!v2v3off) {
	    printf ("Peak at dwell number %d\n", mdwell)
	    printf ("Peak counts: %d\n", x)
	    printf ("X-offset (arcsec): %.3f\n", xoff)
	    printf ("Y-offset (arcsec): %.3f\n", yoff)
	    printf ("V2 offset (arcsec): %.3f\n", v2off)
	    printf ("V3 offset (arcsec): %.3f\n", v3off)
	} else
	    printf ("V2, V3 offsets (arcsec) for %s: {%.3f, %.3f}\n",
		    proot, v2off, v3off)

	# If an output table has been requested, create it.
	if (pos_tab != "") {
	    tmerge (sum//","//pos, merge, "merge", allcols=yes,
		    tbltype="text", allrows=100, extracol=0)

	    print ("dwell i", > cdfile)
	    print ("counts r \"\" counts", >> cdfile)
	    print ("xpos r \"\" arcsec", >> cdfile)
	    print ("ypos r \"\" arcsec", >> cdfile)

	    printf ("root t %s\n", proot, > uparfile)
	    printf ("xoff r %g\n", xoff, >> uparfile)
	    printf ("yoff r %g\n", yoff, >> uparfile)
	    printf ("v2off r %g\n", v2off, >> uparfile)
	    printf ("v3off r %g\n", v3off, >> uparfile)
	    
	    tcreate (pos_tab, cdfile, merge, uparfile=uparfile, nskip=0,
		     nlines=0, nrows=0, hist=yes, extrapar=5,
		     tbltype="default", extracol=0)
	}
	
	# Plot it.
	if (plot) {

	    tstat (pos, "c1", outtable="", lowlim=INDEF, highlim=INDEF,
		   rows="-")
	    xlow = tstat.vmin
	    xhigh = tstat.vmax
	    tstat (pos, "c2", outtable="", lowlim=INDEF, highlim=INDEF,
		   rows="-")
	    ylow = tstat.vmin
	    yhigh = tstat.vmax

	    if (xlow == xhigh) {
		xlow = -1.2
		xhigh = 1.2
	    }
	    if (ylow == yhigh) {
		ylow = -1.2
		yhigh = 1.2
	    }

	    printf ("limits %.6g %.6g %.6g %.6g\n", xlow, xhigh, ylow, yhigh,
		    > script)
	    printf ("location 0.1 0.8 0.1 0.95\n", >> script)
	    printf ("margin 0.3; expand 0.6; box; expand 0.8\n", >> script)
	    printf ("xlabel \"X (arcsec)\"\n", >> script)
	    printf ("ylabel \"Y (arcsec)\"\n", >> script)
	    keypar (d0h, "exptime", silent=yes)
	    if (short_title) {
		printf ("title \"%s\"\n",  aper_fov, >> script)
	    } else {
		if (costar)
		    sx = "Post-Costar"
		else
		    sx = "Pre-Costar"
		x = real (keypar.value)/ np
		printf ("title \"%s %s %s %.2f (sec/dwell) %s\"\n",
			proot, det, aper_fov, x, sx, >> script)
	    }

	    # Create the legend.
	    printf ("vmove .9 .16; expand 1.; justify 8; label \"Legend\"\n",
		    >> script)
	    printf ("vmove .9 .1; expand 2; angle 45; ptype 4 1; dot; ",
		    >> script)
	    printf ("angle 0; expand .7\n", >> script)
	    printf ("vmove .9 .13; justify 8; label \"counts\"\n", >> script)
	    printf ("vmove .89 .1; justify 1; label \"dwell #\"\n", >> script)

	    # Label point, sum and dwell at each point.
	    printf ("define rdot\n", >> script)
	    printf ("lweight 1\nptype 4 1\n", >> script)
	    printf ("expand 1.\nmove &1 &2\nangle 45\ndot\nangle 0\n",
		    >> script)
	    printf ("expand 0.6\njustify 8\nmove &3 &4\nlabel \"&5\"\n",
		    >> script)
	    printf ("justify 1\nmove &6 &7\nlabel \"&8\"\nend\n", >> script)

	    # a new but identical macro as rdot, to avoid an IGI bug 
	    #(JC Hsu, 7/23/96)
	    printf ("define crdot\n", >> script)
	    printf ("lweight 1\nptype 4 1\n", >> script)
	    printf ("expand 1.\nmove &1 &2\nangle 45\ndot\nangle 0\n",
		    >> script)
	    printf ("expand 0.6\njustify 8\nmove &3 &4\nlabel \"&5\"\n",
		    >> script)
	    printf ("justify 1\nmove &6 &7\nlabel \"&8\"\nend\n", >> script)

	    printf ("define brdot\n", >> script)
	    printf ("lweight 3\nptype 4 1\n", >> script)
	    printf ("expand 1.\nmove &1 &2\nangle 45\ndot\n", >> script)
	    printf ("ptype 4 0\ndot\nangle 0\n", >> script)
	    printf ("expand 0.6\njustify 8\nmove &3 &4\nlabel \"&5\"\n",
		    >> script)
	    printf ("justify 1\nmove &6 &7\nlabel \"&8\"\nend\n", >> script)
	    list = sum
	    list2 = pos
	    while (fscan (list, i, j) != EOF) {
		k = fscan (list2, xpos, ypos)
		x = xpos-((xhigh - xlow)/20.)
		y = ypos+((yhigh - ylow)/20.)
		if (i == mdwell)
		    printf ("brdot %.6g %.6g %.6g %.6g %d %.6g %.6g %d\n",
			    xpos, ypos, xpos, y, j, x, ypos, i, >> script)
		else {
		    if (i <= 13) 
		    printf ("rdot %.6g %.6g %.6g %.6g %d %.6g %.6g %d\n",
			    xpos, ypos, xpos, y, j, x, ypos, i, >> script)
		    else
		    printf ("crdot %.6g %.6g %.6g %.6g %d %.6g %.6g %d\n",
			    xpos, ypos, xpos, y, j, x, ypos, i, >> script)
		}
	    }

	    # Vital stats of the plot
	    printf ("lweight 1; justify 6; expand 0.8\n", >> script)
	    printf ("vmove .83 .75; label \"X Stepsize = %.3f\"\n", ds,
		    >> script)
	    printf ("vmove .83 .72; label \"Y Stepsize = %.3f\"\n", ls,
		    >> script)
	    printf ("vmove .83 .65; label \"Offsets:\"\n", >> script)
	    printf ("vmove .85 .62; label \"X = %.3f\"\n", xoff, >> script)
	    printf ("vmove .85 .59; label \"Y = %.3f\"\n", yoff, >> script)
	    printf ("vmove .85 .56; label \"V2 = %.3f\"\n", v2off, >> script)
	    printf ("vmove .85 .53; label \"V3 = %.3f\"\n", v3off, >> script)
	    
	    if (igi_script == "")
		igi (initcmd="", wlpars="", usewcs=no, wcspars="",
		     device=device, metacode="", append=no, debug=no,
		     cursor="", < script)
	}

	# That's all folks.
	delete (tmproot//"*", verify=no, >& "dev$null")
end
