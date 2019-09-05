#
# SHIFTFIND  --  Measures peak position in list of cross-correlation images.
#
#
#
#  Development history:
#  -------------------
#  12-Apr-96: Task created (I.Busko)
#  10-Jun-96: Use new task 'imextreme' to locate crosscor peak.
#  14-Jun-96: Boxsize parameter in imextreme task.
#  14-Mar-97: Box center coordinates in imextreme task.
#  07-Jul-97: Revise calls that rely on the 'imtype' setting.
#  14-Aug-98: Pass original image info (ASF+RH)
#  22-Feb-99: replace x/ycenter with x/yshift (JC Hsu)


procedure shiftfind (cclist, outfile)

char	cclist      = ""     {prompt="Cross-corr image list/template"}
char	outfile     = ""     {prompt="Output file name"}
real	xshift      = 0.     {prompt="Search box X shift (TO the ref image)"}
real	yshift      = 0.     {prompt="Search box Y shift (TO the ref image)"}
real	boxsize     = INDEF  {prompt="Box size where to search for peak"}
real	fwhm        = 7.0    {prompt="Peak FWHM"}
real	ellip       = 0.05   {prompt="Peak ellipticity"}
real	pa          = 45.    {prompt="Peak position angle"}
int	fitbox      = 7      {prompt="Box size used to fit peak"}
bool	kellip      = yes    {prompt="Keep ellipticity fixed ?"}
bool	kpa         = yes    {prompt="Keep position angle fixed ?"}
file	tempdir     = "tmp$" {prompt="Directory for temporary files"}

struct	*filelist            {prompt="Internal parameter, do not use."}
struct	*prlist              {prompt="Internal parameter, do not use."}

char    version = "13Jul2000"{prompt="Date of installation"}

begin
	# These hold input parameters.
	file	ccl, out
	real	xc, yc, box
	# These are for internal use
	file	ccname, temp1, temp2, temp3
	file    tempfile
	char	msg, org_title
	real	xcent, ycent, excent, eycent
	real    xsh, ysh, rot, org_exp
	int     org_group


	# Check for the presence of pre-requisite packages.
	msg = ""
	if (!deftask("tcalc"))      msg = msg // " ttools"
	if (!deftask("pixlocate"))  msg = msg // " imgtools"
	if (!deftask("n2gaussfit")) msg = msg // " fitting"
	if (!deftask("imcntr"))     msg = msg // " proto"
	if (strlen(msg) > 0) {
	    printf ("Please, load the following packages: %s\n", msg)
	    bye
	}

	# Read task parameters.
	ccl                 = cclist
	out                 = outfile
	xc                  = xshift
	yc                  = yshift
	box                 = boxsize
	tgausspars.a        = 0.0
	tgausspars.fwhm     = fwhm
	tgausspars.ellip    = ellip
	tgausspars.theta    = pa
	tgausspars.boxsize  = fitbox
	tgausspars.va       = no
	tgausspars.vampl    = yes
	tgausspars.vxcent   = yes
	tgausspars.vycent   = yes
	tgausspars.vfwhm    = yes
	tgausspars.vellip   = !kellip
	tgausspars.vtheta   = !kpa
	n2gaussfit.averbose = no
	controlpars.method  = "amoeba"

	# Create temporary file names.
	tempfile = tempdir // "dither"
	temp1 = mktemp (tempfile)
	temp2 = mktemp (tempfile)
	temp3 = mktemp (tempfile)

	# Reset output stream.
	printf ("", > out)

	# Make list with input files
	sections (ccl, option="root", > temp3)

	# Get center of cross-corr image.
	filelist = temp3
	i = fscan (filelist, ccname)

	# use crpix instead of naxis (7/13/2000 JC Hsu)
	#imgets (ccname, "i_naxis1", mode="h")
	#xcent = real(imgets.value) / 2. + 1.
	#imgets (ccname, "i_naxis2", mode="h")
	#ycent = real(imgets.value) / 2. + 1.
        imgets (ccname, "crpix1", mode="h")
        xcent = real(imgets.value)
        imgets (ccname, "crpix2", mode="h")
        ycent = real(imgets.value)

	# Note the sign change, since the shift is TO the reference image
	if (xc != INDEF) xc = xcent - xc
	if (yc != INDEF) yc = ycent - yc

	# Do for each image in input list:
	prlist   = temp3
	filelist = temp3
	while (fscan (filelist, ccname) != EOF) {

	    # Brightest pixel gives peak starting position and amplitude.
	    imextreme (ccname, boxsize=box, x=xc, y=yc, verbose-, mode="h")
	    tgausspars.ampl  = imextreme.max
	    tgausspars.xcent = real(imextreme.xmax)
	    tgausspars.ycent = real(imextreme.ymax)

	    # Fit 2-D gaussian to peak.
	    n2gaussfit (ccname, temp2, rt-, resample+, errtype="uniform", \
                        verbose+, mode="h")

	    # Get results.
	    imgets (ccname, "i_naxis1", mode="h")
	    tabpar (temp2, "coeff3", 1, mode="h")
	    xsh =  real (tabpar.value)
	    xsh = -(xsh - xcent)
	    tabpar (temp2, "err3", 1, mode="h")
	    excent = real (tabpar.value)
	    tabpar (temp2, "coeff4", 1, mode="h")
	    ysh = real (tabpar.value)
	    ysh = -(ysh - ycent)
	    tabpar (temp2, "err4", 1, mode="h")
	    eycent = real (tabpar.value)

	    # Output.
	    # STDOUT
	    i = fscan (prlist, ccname)
	    printf ("Crosscor: %s\n", ccname)
	    printf ("Xshift = %9.4f   Error = %6.4f\n", xsh, excent)
	    printf ("Yshift = %9.4f   Error = %6.4f\n\n", ysh, eycent)
	    # File
            imgets(ccname,"ROTANGLE",mode="h")
	    rot = real(imgets.value)
            imgets(ccname,"ORG_TITL",mode="h")
            org_title = imgets.value
	    if (org_title == "0") 
		org_title = ccname
	    imgets(ccname,"ORG_GRP",mode="h")
	    org_group = int(imgets.value)
            imgets(ccname,"ORG_EXP",mode="h")
            org_exp = real(imgets.value)
	    printf ("%s  %1d  %9.4f %6.4f %9.4f %6.4f %7.2f %9.2f\n", org_title, 
		    org_group, xsh, excent, ysh, eycent, rot, org_exp, >> out)

	    # Delete temporaries.
	    delete  (temp1, verify-, >& "dev$null")
	    delete  (temp2 // ".tab", verify-, >& "dev$null")
	}
	delete  (temp3, verify-, >& "dev$null")
	filelist = ""
	prlist   = ""
end
