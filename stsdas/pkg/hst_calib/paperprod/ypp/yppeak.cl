procedure ypeakup (root, imtype)

char	root	{prompt="Rootname of peakup observation"}
char	imtype	{prompt="Image type of root (FITS or GEIS)"}
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
	real	xcomp, ycomp, compsize	# values for placing the compass
	real	orientat		# orientation keyword value
	real	xaxes, yaxes		# length of X/Y axes on compass
	real	aperoffx, aperoffy	# initial offset at beginning of scan
	char	aperid			# aperture name from header
	real	mjd_date		# use this date to see if pre-COSTAR
	char	ftype			# What type of image is it: FITS or GEIS
	int	gcount, grp

	# Get interactive parameters
	proot = root
	ftype = imtype

	# Temporary files.
	tmproot = mktemp ("tmp$PPYPEAK")
	cdfile = tmproot//"_cd"
	merge = tmproot//"_mrg"
	pos = tmproot//"_pos"
	if (igi_script == "")
	    script = tmproot//"_script.igi"
	else
	    script = igi_script
	sum = tmproot//"_sum"
	sum2 = tmproot//"_sum2"
	uparfile = tmproot//"_upar"

	# Set other file names.
	if (ftype == "geis") {
		d0h = proot//".d0h"
		shp = proot//".shh"
	} else {
		d0h = proot//"_d0f.fits[0]"
		shp = proot//"_shf.fits[0]"
	}

	# First, let's determine whether this is a COSTAR-corrected
	# observation with the APER_FOV keyword
	# The check will be made based on MJD: date < 49353., pre-COSTAR
	keypar (d0h, "fpkttime", silent=yes)
	mjd_date = real (keypar.value)
	
	# Get relevant keywords.
	
	# Create a table with the sum of the counts for each group
	# of the peakup.
	if (ftype == "geis") {
		gstatistics (d0h, masks="", groups="*", g_accum=no,
		     fields="sum", lower=INDEF, upper=INDEF,
		     gstpar="", > sum2)
	} else {
		# FITS files are 3-D, so we must access it differently...
		keypar (d0h, "naxis3", silent=yes)
		gcount = int (keypar.value)
		grp = 1
		while (grp <= gcount) {
		    gstatistics (d0h//"[*,*,"//grp//"]", masks="", 
			groups="*", g_accum=no,
		     	fields="sum", lower=INDEF, upper=INDEF,
		     	gstpar="", >> sum2)
		    grp = grp + 1
		}
    	}

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

	if (ftype == "geis") {
	tprint (sum2, prparam=no, prdata=yes, pwidth=80, plength=0,
		showrow=no, showhdr=no, showunits=no, columns="",
		rows="-", option="plain", align=yes, sp_col="", lgroup=0,
		> sum)
	} else {
	tprint (sum2, prparam=no, prdata=yes, pwidth=80, plength=0,
		showrow=yes, showhdr=no, showunits=yes, columns="c1",
		rows="-", option="plain", align=yes, sp_col="", lgroup=0,
		> sum)

	}
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

	# Get info on aperoff for new algorith
	# Initial offset performed at beginning of spatial scan
	# values are in object space.
	keypar (shp, "aperoffx", silent=yes)
	aperoffx = real (keypar.value)
	keypar (shp, "aperoffy", silent=yes)
	aperoffy = real (keypar.value)
	keypar (d0h, "aper_id", silent=yes)
	aperid = keypar.value

	# build aper_fov string depending on pre-COSTAR or not
	if (mjd_date > 49353. ) {	
		keypar (d0h, "aper_fov", silent=yes)
		aper_fov = keypar.value
	} else {
		aper_fov = "pre-COSTAR "//aperid
	}

	# Calculate dwell and line spacing.
	ds = sl / real (dpl - 1)
	if (lps > 1)
	    ls = sw / real (lps - 1)
	else
	    ls = 0

	# Look in the SHP data for moving target information.
	if (ftype == "geis") {
		listpix (shp//"[3][880:880]", wcs="logical",
			formats="", verbose=no) | scan (i, new)
	} else {
		listpix (shp//"[880:880,3]", wcs="logical",
			formats="", verbose=no) | scan (i, new)
	}		
	if (new == 0 && np == 3) {
	    print ("Moving Target Peakup.")
	    rho = 0
	}

	# New algorithm here (WJH, 9-Jan-1997)
	# Sometimes that scan parameters are not populated
	# flag v2,v3 with 666 and exit
	if ( (ds <= 0.) || (np <= 0.) ) {
		v2off = 666.
		v3off = 666.
	}

	# Find dwell with the maximum counts.
	list = sum
	i = fscan (list, mdwell, x)
	while (fscan (list, i, y) != EOF)
	    if (y > x) {
		mdwell = i
		x = y
	    }

	if ( (aperid == "C-4") || (aperid == "C-3") ) {
		temp = aperoffx
		aperoffx = aperoffy
		aperoffy = -temp
		# reset peakup to peakdown
		list = sum
		i = fscan (list, mdwell, x)
		while (fscan (list, i, y) != EOF)
	  	    if (y < x) {
			mdwell = i
			x = y
		    }
	}
	
	# determine scan pattern of spacecraft offsets and motions wrt nominal
	# target position
	# First, insure algorithm is not fooled by non-zero zeros like 0.5e-15
	if ( abs(aperoffx) < 1.0e-6 ) aperoffx = 0.
	if ( abs(aperoffy) < 1.0e-6 ) aperoffy = 0.
	
	# all non-bar, 2-D scans always have offset object -X, -Y
	# single line scans of the SLIT are -X, 0
	if ( (aperoffx < 0.) && (aperoffy <= 0.) )  
		rho = 270.
	else 
	
	# Single line scans along Y since day 170, 1995, offset object -Y
	# also, PEAKDOWNS in the BARs
	     if ( (aperoffx == 0.) && (aperoffy<= 0.) )
		rho = 180. 
	else 
	
	# Single line scans along Y before day 170, 1995, offset object +Y
	    if ( (aperoffx == 0.) && (aperoffy > 0.) )
		rho = 0. 
	else
	    print ("Unrecognized Scan Pattern\n")
	

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

	# Superfluous case, remains for historical reasons
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

	    printf("dwell i", > cdfile)
	    printf("counts r \"\" counts", >> cdfile)
	    printf("xpos r \"\" arcsec", >> cdfile)
	    printf ("ypos r \"\" arcsec", >> cdfile)

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

	    printf ("limits %.6g %.6g %.6g %.6g\n", xhigh, xlow, ylow, yhigh,
		    > script)
	    printf ("location 0.1 0.8 0.1 0.95\n", >> script)
	    printf ("margin 0.3; expand 0.6; box; expand 0.8\n", >> script)
	    printf ("xlabel 'X (arcsec)'\n", >> script)
	    printf ("ylabel 'Y (arcsec)'\n", >> script)
	    keypar (d0h, "exptime", silent=yes)
	    if (short_title) {
		printf ("title '%s'\n",  aper_fov, >> script)
	    } else {
		if (costar)
		    sx = "Post-Costar"
		else
		    sx = "Pre-Costar"
		x = real (keypar.value)/ np
		printf ("title '%s %s %s %.2f (sec/dwell) %s'\n",
			proot, det, aper_fov, x, sx, >> script)
	    }

	    # Create the legend.
	    printf ("vmove .9 .16; expand 1.; justify 8; label 'Legend'\n",
		    >> script)
	    printf ("vmove .9 .1; expand 2; angle 45; ptype 4 1; dot; ",
		    >> script)
	    printf ("angle 0; expand .7\n", >> script)
	    printf ("vmove .9 .13; justify 8; label 'counts'\n", >> script)
	    printf ("vmove .89 .1; justify 1; label 'dwell #'\n", >> script)

	    # Label point, sum and dwell at each point.
	    list = sum
	    list2 = pos
	    while (fscan (list, i, j) != EOF) {
		k = fscan (list2, xpos, ypos)
		x = xpos-((xhigh - xlow)/20.)
		y = ypos+((yhigh - ylow)/20.)
		if (i == mdwell) {
	    	    printf ("lweight 3\nptype 4 1\nexpand 1.\n", >> script)
	    	    printf ("move %.6g %.6g\nangle 45\ndot\n", 
				xpos, ypos, >> script)
	    	    printf ("ptype 4 0\ndot\nangle 0\n", >> script)
		} else {
	    	    printf ("lweight 1\nptype 4 1\nexpand 1.\n", >> script)
	    	    printf ("move %.6g %.6g\nangle 45\ndot\nangle 0\n",
		    	    	xpos, ypos, >> script)
		}
	    	printf ("expand 0.6\njustify 8\nmove %.6g %.6g\n", 
				xpos, y, >> script)
	        printf ("label '%d'\njustify 1\nmove %.6g %.6g\nlabel '%d'\n", 
				j, x, ypos, i, >> script)
	    }

	
	# Add the compass with X/Y axes here...
	xcomp = 0.9	
	ycomp = 0.35
	compsize = 0.05
	keypar(d0h, "orientat", silent=yes)
	orientat = real (keypar.value)

	printf("lweight 1\n", >> script)
	t_compass (script, xcomp, ycomp, compsize, orientat, yes)

	# Now put in the X and Y axes on the compass
	xaxes = xcomp - compsize
	yaxes = ycomp + compsize*1.5

	# Add the X and Y axes to the compass...
	printf("expand 0.7\n", >> script)
	printf("move %.2g %.2g; draw %.2g %.2g\n",xcomp, ycomp, xaxes, ycomp, >> script)
	xaxes = xcomp - compsize - (compsize / 5.)
	printf("move %.2g %.2g; label 'X'\n", xaxes, ycomp, >> script)

	printf("move %.2g %.2g; draw %.2g %.2g\n",xcomp, ycomp, xcomp, yaxes, >> script)
	yaxes = yaxes + (compsize / 5.)
	xaxes = xcomp + (compsize / 5.)
	printf("move %.2g %.2g; label 'Y'\n", xaxes, yaxes, >> script)
	


	    # Vital stats of the plot
	    printf ("lweight 1; justify 6; expand 0.8\n", >> script)
	    printf ("vmove .83 .75; label 'X Stepsize = %.3f'\n", ds,
		    >> script)
	    printf ("vmove .83 .72; label 'Y Stepsize = %.3f'\n", ls,
		    >> script)
	    printf ("vmove .83 .65; label 'Offsets:'\n", >> script)
	    printf ("vmove .85 .62; label 'X = %.3f'\n", xoff, >> script)
	    printf ("vmove .85 .59; label 'Y = %.3f'\n", yoff, >> script)
	    printf ("vmove .85 .56; label 'V2 = %.3f'\n", v2off, >> script)
	    printf ("vmove .85 .53; label 'V3 = %.3f'\n", v3off, >> script)
	    
	    if (igi_script == "")
		igi (initcmd="", wlpars="", usewcs=no, wcspars="",
		     device=device, metacode="", append=no, debug=no,
		     cursor="", < script)
	}

	# That's all folks.
	delete (tmproot//"*", verify=no, >& "dev$null")
end
