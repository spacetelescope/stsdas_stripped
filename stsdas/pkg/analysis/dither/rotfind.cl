# ROTFIND  --  Scan family of cross-correlation images built by CROSSDRIZ
#              task. Build table with peak value in each image, and analyse 
#              it by fitting polinomial.
#
#
#  Development history:
#  -------------------
#  18-Nov-95: Task created                                     (I.Busko)
#  21-Nov-95: Automatic starting values in polynomial fit.
#  23-Nov-95: Higher order terms in polynomial fit.
#  24-Nov-95: Search peak in box only; optional direct input
#             of table with angle-peak pairs.
#  28-Nov-95: Fit Gaussian to correlation peak.
#  29-Nov-95: Find linear shift by interpolation.
#  02-Dec-95: Error computation.
#  05-Dec-95: Non-interactive mode.
#  22-Dec-95: ASCII-only output..
#  05-Feb-96: Increased stability comes from fitting to two bracketing 
#             peaks starting from former solution instead of from former
#             initial guess. Added flags for keeping Gaussian shape const.
#  06-Feb-96: Added derivatives (shift / rotation).
#  16-Feb-96: Changed sign of x-y linear shifts.
#  12-Apr-96: Initial correlation peak is brightest pixel.
#  05-Jun-96: Use new task 'imextreme' to locate crosscor peak.
#  07-Jun-96: Got rid of 'partab' (crashed when dealing with large numbers).
#  14-Jun-96: Boxsize parameter in imextreme task.
#  14-Mar-97: Box center coordinates in imextreme task.
#  08-Apr-97: Got rid of "extension" parameter.
#  07-Jul-97: Revise calls that rely on the 'imtype' setting.

procedure rotfind (input, output)

char	input       = ""      {prompt="Cross-corr image template/list"}
char	output      = ""      {prompt="Output file name"}
real	xcenter     = INDEF   {prompt="Search box X position"}
real	ycenter     = INDEF   {prompt="Search box Y position"}
real	boxsize     = INDEF   {prompt="Box size where to search for peak"}
real	fwhm        = 7.0     {prompt="Peak FWHM"}
real	ellip       = 0.05    {prompt="Peak ellipticity"}
real	pa          = 45.     {prompt="Peak position angle"}
int	fitbox      = 7       {prompt="Box size used to fit peak"}
bool	kellip      = yes     {prompt="Keep ellipticity fixed ?"}
bool	kpa         = yes     {prompt="Keep position angle fixed ?"}
char	intable     = ""      {prompt="Input table name"}
file	tempdir     = "tmp$"  {prompt="Directory for temporary files"}
bool	interactive = yes     {prompt="Interactive ?"}
bool	verbose     = yes     {prompt="Verbose ?"}

struct	*filelist  	      {prompt="Internal parameter, do not use."}

char    version  = "13Jul2000"{prompt="Date of installation"}

begin
	# These hold input parameters.
	file	root, in, out
	real	xc, yc, ell, posa, box
	bool	pak, ellk
	bool	inter, verb
	# These are for internal use
	file	temp1, temp3, temp5
	file	temp6, temp7, temp8, temp9, tempfile
	file	filename, im1, im2
	char	imname, tstr, msg
	int	i, j, k, kk, ncc
	real	xp, yp, xcent, ycent
	real	rbest, rerr, r1, r2, ar1, ar2
	real	x1, x2, y1, y2, aux
	real	ex1, ex2, ey1, ey2
	real	xshift, yshift
	real	exshift, eyshift
	real	dxdr, dydr
	bool	bracket

	# Check for the presence of pre-requisite packages.
	msg = ""
	if (!deftask("imcntr")) msg = msg // " proto"
	if (!deftask("nfit1d")) msg = msg // " fitting"
	if (!deftask("tcalc"))  msg = msg // " ttools"
	if (strlen(msg) > 0) {
	    printf ("Please, load the following package(s):  %s\n", msg)
	    bye
	}

	# Read task parameters.
	root  = input
	out   = output
	xc    = xcenter
	yc    = ycenter
	ell   = ellip
	posa  = pa
	box   = boxsize
	ellk  = kellip
	pak   = kpa
	in    = intable
	inter = interactive
	verb  = verbose

	# Make temporary file names.
	tempfile = tempdir // "dither"
	temp1 = mktemp (tempfile)
	temp3 = mktemp (tempfile)
	temp5 = mktemp (tempfile)
	temp6 = mktemp (tempfile)
	temp7 = mktemp (tempfile)
	temp8 = mktemp (tempfile)
	temp9 = mktemp (tempfile)

	# Test existence of already built table with angle-peak pairs.
	if (strlen(in) == 0) {

	    # Table wasn't supplied: build one by scanning all images
	    # in input rootname family.
	    if (verb)
	        print ("Making file list...")

	    # Make list with all files in rootname family.
	    sections (root, option="fullname", > temp1)

	    if (verb)
	        print ("Making table with rough peak values...")

	    # Get center of cross-corr image.
	    filelist = temp1
	    i = fscan (filelist, filename)

            # use crpix instead of naxis (7/13/2000 JC Hsu) 
	    #imgets (filename, "i_naxis1", mode="h")
	    #xcent = real(imgets.value) / 2. + 1.
	    #imgets (filename, "i_naxis2", mode="h")
	    #ycent = real(imgets.value) / 2. + 1.
            imgets (filename, "crpix1", mode="h") 
            xcent = real(imgets.value) 
            imgets (filename, "crpix2", mode="h") 
            ycent = real(imgets.value) 


	    # Make table with peak value in each cross-corr image.
	    filelist = temp1
	    while (fscan (filelist, filename) != EOF) {
	        imextreme (filename, boxsize=box, x=xc, y=yc, verbose-, mode="h")
	        print (filename, "  ", imextreme.xmax, "  ", imextreme.ymax,
                      "  ", imextreme.max, >> temp6)
	    }

	    if (verb)
	        print ("Updating peak values with Gaussian fit...")

	    # Fit a 2-D gaussian to each peak, using imstat and imcntr
	    # results as first guess.
	    tgausspars.a       = 0.0
	    tgausspars.fwhm    = 7.
	    tgausspars.ellip   = ell
	    tgausspars.theta   = posa
	    tgausspars.boxsize = fitbox
	    tgausspars.va      = no
	    tgausspars.vampl   = yes
	    tgausspars.vxcent  = yes
	    tgausspars.vycent  = yes
	    tgausspars.vfwhm   = yes
	    tgausspars.vellip  = !ellk
	    tgausspars.vtheta  = !pak
	    controlpars.method = "amoeba"
	    tinfo (temp6, ttout-, mode="h")
	    for (i=1; i <= tinfo.nrows; i=i+1) {
	        if (verb) {
	            tabpar (temp6, "c1", i, mode="h")
	            imgets (tabpar.value, "ROTANGLE", mode="h")
	            print ("\nFitting peak for rotation angle = ", \ 
	                    real(imgets.value), "degree.")
	        }
	        tabpar (temp6, "c2", i, mode="h")
	        tgausspars.xcent = real(tabpar.value)
	        tabpar (temp6, "c3", i, mode="h")
	        tgausspars.ycent = real(tabpar.value)
	        tabpar (temp6, "c4", i, mode="h")
	        tgausspars.ampl    = real(tabpar.value)
	        tabpar (temp6, "c1", i, mode="h")
	        n2gaussfit (tabpar.value, temp5, rt-, resample-, verbose=verb, \
	                    mode="h")
	    }
	    prfit (temp5, mode="h", > out)

	    if (verb)
	        print ("\nAdding column with rotation angles...")

	    # Add column with rotation angles.
	    tinfo (temp6, ttout-, mode="h")
	    for (i=1; i <= tinfo.nrows; i=i+1) {
	        tabpar (temp6, "c1", i, mode="h")
	        imgets (tabpar.value, "ROTANGLE", mode="h")
	        tabpar (temp5, "coeff2", i, mode="h")
	        print (real(imgets.value), " ", real(tabpar.value), >> temp9) 
	    }
	    delete  (temp6, verify-, >& "dev$null")
	    temp6 = temp9
	    tsort (temp6, "c1", ascend+ , mode="h")
	} else
	
	    # Table supplied: just read it.
	    temp6 = in

	if (verb)
	    print ("Fitting Legendre polynomial...")

	# Now the fit begins. First fit second-order polynomial, just to 
	# get starting values for iterative fitting routine.
	gfit1d (temp6 // " c1 c2", temp3, interact-, function="legendre", \
	        order=3, >& "dev$null")

	# Set function to be fitted. Maximum allowable order depends
	# on number of available data points (cross-corr images).
	tinfo (temp6, ttout-, mode="h")
	ncc = int(tinfo.nrows)
	switch (ncc) {
	case 1, 2, 3:
	    error (1, "Insufficient number of cross-correlation images.")
	case 4:
	    {
	    userpars.function = "c2+c3*(x-c1)**2"
	    userpars.c4 = INDEF
	    userpars.c5 = INDEF
	    userpars.c6 = INDEF
	    userpars.c7 = INDEF
	    }
	case 5:
	    {
	    userpars.function = "c2+c3*(x-c1)**2+c4*(x-c1)**3"
	    userpars.c4 = 0.0
	    userpars.c5 = INDEF
	    userpars.c6 = INDEF
	    userpars.c7 = INDEF
	    userpars.v4 = no
	    }
	default:
	    {
	    userpars.function = "c2+c3*(x-c1)**2+c4*(x-c1)**3+c5*(x-c1)**4"
	    userpars.c4 = 0.0
	    userpars.c5 = 0.0
	    userpars.c6 = INDEF
	    userpars.c7 = INDEF
	    userpars.v4 = no
	    userpars.v5 = no
	    }
#	default:
#	    {
#	    userpars.function = "c2+c3*(x-c1)**2+c4*(x-c1)**4+c5*(x-c1)**6\
#                                +c6*(x-c1)**8"
#	    userpars.c4 = 0.0
#	    userpars.c5 = 0.0
#	    userpars.c6 = 0.0
#	    userpars.c7 = INDEF
#	    userpars.v4 = no
#	    userpars.v5 = no
#	    userpars.v6 = no
#	    }
	}

	if (verb)
	    print ("Finding angle offset in function...")

	# Use c2 (constant) and c3 (quadratic) starting values 
	# from Legendre polynomial fit.
	tabpar (temp3, "coeff1", 1, mode="h")
	userpars.c2 = real(tabpar.value)
	tabpar (temp3, "coeff3", 1, mode="h")
	userpars.c3 = real(tabpar.value)
	userpars.c1 = 0.1

	# Fit only the angle zero point, c1.
	userpars.v1 = yes
	userpars.v2 = no
	userpars.v3 = no

	nfit1d (temp6 // " c1 c2", temp3, interact-, verbose-, \
	        function="user", >& "dev$null")

	if (verb)
	    print ("Fitting function...")

	# Now, fit all three coefficients.
	tabpar (temp3, "coeff1", 2, mode="h")
	userpars.c1 = real(tabpar.value)
	tabpar (temp3, "coeff2", 2, mode="h")
	userpars.c2 = real(tabpar.value)
	tabpar (temp3, "coeff3", 2, mode="h")
	userpars.c3 = real(tabpar.value)
	userpars.v3 = yes
	userpars.v2 = yes
	userpars.v1 = yes
	nfit1d (temp6 // " c1 c2", temp3, interact-, verbose-, \
	        function="user", >& "dev$null")

	# Call fitting routine in interactive mode, if requested.
	tabpar (temp3, "coeff1", 3, mode="h")
	userpars.c1 = real(tabpar.value)
	tabpar (temp3, "coeff2", 3, mode="h")
	userpars.c2 = real(tabpar.value)
	tabpar (temp3, "coeff3", 3, mode="h")
	userpars.c3 = real(tabpar.value)
	userpars.v3 = yes
	userpars.v2 = yes
	userpars.v1 = yes
	copy (temp6, tempdir // "input", verbose+, mode="h")
	nfit1d (tempdir // "input c1 c2", temp3, interact=inter, verbose-, \
	        function="user", ltype="boxes", gcur="")
	delete  (tempdir // "input", verify-)

	# Fit angle offset with error estimation turned on.
	tabpar (temp3, "coeff1", 4, mode="h")
	userpars.c1 = real(tabpar.value)
	userpars.v1 = yes
	tabpar (temp3, "coeff2", 4, mode="h")
	userpars.c2 = real(tabpar.value)
	userpars.v2 = no
	tabpar (temp3, "coeff3", 4, mode="h")
	userpars.c3 = real(tabpar.value)
	userpars.v3 = no
	if (ncc >= 5) {
	    tabpar (temp3, "coeff4", 4, mode="h")
	    userpars.c4 = real(tabpar.value)
	    userpars.v4 = no
	}
	if (ncc >= 6) {
	    tabpar (temp3, "coeff5", 4, mode="h")
	    userpars.c5 = real(tabpar.value)
	    userpars.v5 = no
	}
#	if (ncc >= 7) {
#	    tabpar (temp3, "coeff6", 4, mode="h")
#	    userpars.c6 = real(tabpar.value)
#	    userpars.v6 = no
#	}

	nfit1d (temp6 // " c1 c2", temp3, interact-, verbose-, \
	        function="user", resample+, errtype="uniform",   \
	        low_reject=3.5, high_reject=3.5, niterate=1,   \
	        >& "dev$null")

	tinfo (temp3, ttout-, mode="h")
	tabpar (temp3, "coeff1", (tinfo.nrows-1), mode="h")
	rbest = real (tabpar.value)
	tabpar (temp3, "err1", tinfo.nrows, mode="h")
	rerr = real (tabpar.value)
	print ("\nRotation angle = ", rbest, " degrees.\tError = ", rerr)

	# Compute linear shift. If angle X peak table was input
	# by user, stops here.
	if (strlen(in) == 0) {

	    if (verb)
	        print ("\nNow, find linear shift by interpolation...")

	    # Make table that associates file names to rotation angles.
	    hedit ("@" // temp1, "ROTANGLE", "", \
	            add-, delete-, show+, update-, verify-, > temp8)
	    tsort (temp8, "c2", ascend+ , mode="h")

	    # Scan table, find rotation angles that bracket solution.
	    j = 1
	    bracket = no
	    tinfo (temp8, ttout-, mode="h")
	    for (i=1; i <= tinfo.nrows-1; i=i+1) {
	        tabpar (temp8, "c2", i, mode="h")
	        r1 = real (tabpar.value)
	        k = i + 1
	        tabpar (temp8, "c2", k, mode="h")
	        r2 = real (tabpar.value)
	        if ((r1 <= rbest) && (r2 > rbest)) {
	            j = i
	            ar1 = r1
	            ar2 = r2
	            bracket = yes
	        }
	    }
	    if (bracket) {
	        r1 = ar1
	        r2 = ar2
	    } else
	        error (0, "Invalid angle solution.")

	    # Get corresponding image names.
	    tabpar (temp8, "c1", j, mode="h")
	    im1 = substr (tabpar.value, 1, strlen(tabpar.value)-10)
	    k = j + 1
	    tabpar (temp8, "c1", k, mode="h")
	    im2 = substr (tabpar.value, 1, strlen(tabpar.value)-10)

	    # Find corresponding lines in Gaussian solution table.
	    tinfo (temp5, ttout-, mode="h")
	    for (k=1; k <= tinfo.nrows; k=k+1) {
	        tabpar (temp5, "file", k, mode="h")
	        kk = stridx ("[", tabpar.value)
	        if (kk > 2)
	            tstr = substr (tabpar.value, 1, kk-1)
	        else
	            tstr = tabpar.value
	        if (tstr == im1)
	            i = k
	        if (tstr == im2)
	            j = k
	    }
	    # Fit peak position in both cross-corr images. This has to
	    # be re-done just to get the fit errors. Use as starting
	    # guess the solution from previous fit.
	    tabpar (temp5, "coeff2", i, mode="h")
	    tgausspars.ampl = real(tabpar.value)
	    tabpar (temp5, "coeff3", i, mode="h")
	    tgausspars.xcent = real(tabpar.value)
	    tabpar (temp5, "coeff4", i, mode="h")
	    tgausspars.ycent = real(tabpar.value)
	    n2gaussfit (im1, temp7, rt-, resample+, \
	                errtype="uniform", verbose=verb, mode="h")
	    tabpar (temp5, "coeff2", j, mode="h")
	    tgausspars.ampl = real(tabpar.value)
	    tabpar (temp5, "coeff3", j, mode="h")
	    tgausspars.xcent = real(tabpar.value)
	    tabpar (temp5, "coeff4", j, mode="h")
	    tgausspars.ycent = real(tabpar.value)
	    n2gaussfit (im2, temp7, rt-, resample+, \
	                errtype="uniform", verbose=verb, mode="h")

	    # Interpolate. Autocorrelation peak sits on image center.
	    aux = (rbest - r1)/(r2 -r1)
	    tabpar (temp7, "coeff3", 1, mode="h")
	    x1 = real (tabpar.value)
	    tabpar (temp7, "err3", 1, mode="h")
	    ex1 = real (tabpar.value)
	    tabpar (temp7, "coeff3", 2, mode="h")
	    x2 = real (tabpar.value)
	    tabpar (temp7, "err3", 2, mode="h")
	    ex2 = real (tabpar.value)
	    xshift = (x2 - x1) * aux + x1
	    xshift = -(xshift - xcent)
	    exshift = sqrt (ex2**2 + ex1**2)
	    dxdr = (x2 - x1) / (r2 - r1)

	    tabpar (temp7, "coeff4", 1, mode="h")
	    y1 = real (tabpar.value)
	    tabpar (temp7, "err4", 1, mode="h")
	    ey1 = real (tabpar.value)
	    tabpar (temp7, "coeff4", 2, mode="h")
	    y2 = real (tabpar.value)
	    tabpar (temp7, "err4", 2, mode="h")
	    ey2 = real (tabpar.value)
	    yshift = (y2 - y1) * aux + y1
	    yshift = -(yshift - ycent)
	    eyshift = sqrt (ey2**2 + ey1**2)
	    dydr = (y2 - y1) / (r2 - r1)

	    # Final output.
	    # STDOUT
	    print ("\nFinal solution:  Xshift   = ", xshift, "\tError = ", exshift)
	    print ("                 Yshift   = ", yshift, "\tError = ", eyshift)
	    print ("                 Rotation = ", rbest, "\t\tError = ", rerr)
	    print ("                 dx/dr    = ", dxdr, "\tdy/dr  = ", dydr)

	    # File
	    print ("\nFinal solution:  Xshift   = ", xshift, "\tError = ", \
	            exshift, >> out)
	    print ("                 Yshift   = ", yshift, "\tError = ",   \
	            eyshift, >> out)
	    print ("                 Rotation = ", rbest, "\t\tError = ",  \
	            rerr, >> out)
	    print ("                  dx/dr   = ", dxdr, "\tdy/dr  = ", dydr,
	           >> out)
	}

	# Delete temporaries.
	delete  (temp1, verify-, >& "dev$null")
	delete  (temp6, verify-, >& "dev$null")
	delete  (temp8, verify-, >& "dev$null")
	delete  (temp9, verify-, >& "dev$null")
	delete  (temp3 // ".tab", verify-, >& "dev$null")
	delete  (temp5 // ".tab", verify-, >& "dev$null")
	delete  (temp7 // ".tab", verify-, >& "dev$null")
	filelist = ""
end
