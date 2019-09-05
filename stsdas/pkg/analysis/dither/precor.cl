#
#  PRECOR  --
#
#
#  BEWARE: This code is full of last-minute workarounds to make it work
#  with FITS files under a buggy FITS kernel. The original code was
#  WFPC-centric and needs a substantial redesign and rewrite once the
#  FITS kernel gets stabilized.
#
#
#  Revision history:
#  ----------------
#
#  20 Aug 97  -  Implemented, from a script by A. Fruchter (I. Busko)
#  15 Oct 97  -  Removed do_sqrt (IB)
#  16 Oct 97  -  Replaced pickfile by struct * parameter. Pickfile gets
#                confused with the naming convention (IB)
#  27 Oct 97  -  Renamed cgsky to sky (IB)
#  11 Nov 97  -  Put back do_sqrt parameter and rename it sig2n (IB)
#  29 May 98  -  Empty extension name forces use of imtype (IB)
#  22 Jun 98  -  Supports .imh files (IB)
#  01 Oct 98  -  Skyname parameter is passed to sky task (IB)

procedure precor (inlist)

char	inlist      	{prompt="Image(s) for cosmic ray cleaning"}
char	box_siz = "5"	{prompt="Convolution box size -- 3 or 5 pixels"}
int	min_pix = 16	{prompt="Required number of significant pixels"} 
real	min_val = 3.0	{prompt="Minimum value considered significant"}
int     ngroup  = 4	{prompt="number of groups in image"}
bool	do_sky  = yes	{prompt="Run 'sky' on the input image?"}
char    skyname ="BACKGRND"{prompt="Header parameter to be updated with sky"}
bool	sig2n   = no	{prompt="Estimate s/n of each pixel?"}
bool	verbose = no	{prompt="Verbose output?"}
file	tempdir = "tmp$"{prompt="Directory for temporary files"}
struct	*filelist   	{prompt="Internal parameter, do not use."}
char	version = "Jul132000"	{prompt=">Date of installation"}

begin
	char	t_listin, t_kernsz, t_skyn
	char	infile,img,simg,hold,hold2,outimg,imcalc_in,outname
        char	in_name,out_name,holdfile,hold1,imstr,find_kern,save_kern
	char	imcals, msg, imtp
	file	t_tempdir, t_list, temp1
	int	t_minpix, t_ngroup
	int	ii, jj, f1
	real	t_minval
	bool	t_verb, t_dosky, t_dosqrt

	# Check for presence of pre-requisite tasks/packages.
	msg = ""
	if (!deftask ("convolve")) msg = msg // " images"
	if (!deftask ("gcopy"))    msg = msg // " imgtools"
	if (strlen (msg) > 0) {
	    printf ("Please, load packages: %s\n", msg)
	    bye
	}

	# Get input parameters.
	t_listin  = inlist
	t_kernsz  = box_siz
	t_minpix  = min_pix
	t_minval  = min_val
	t_ngroup  = ngroup
	t_dosky   = do_sky
	t_skyn    = skyname
	t_dosqrt  = sig2n
	t_verb    = verbose
	t_tempdir = tempdir

	# Set up kernels
	if (t_kernsz == "3") {
            if (t_minpix > 9) {
                error (0, "if box_siz = 3 then min_pix must be <= 9")
            }
	    find_kern="1 1 1"
	    save_kern = "1 1 1 1 1"
	} else if (t_kernsz == "5") {
            if (t_minpix > 25) {
                error (0, "if box_siz = 5 then min_pix must be <= 9")
            }
	    find_kern="1 1 1 1 1" 
	    save_kern = "1 1 1 1 1 1 1"
	} else
	    error (0, "kernel size must be 3 or 5")

	# Get imtype.
	show ("imtype") | scan (imtp)
	if (substr (imtp,1,1) != ".")
	    imtp = "." // imtp

	# Loop through input files:
	t_list = mktemp (t_tempdir // "pre_cor")
	sections (t_listin, option="fullname", > t_list)
	filelist = t_list
	while (fscan (filelist, img) != EOF) {

#	countfile (t_inlist)
#	for (f1 = 1; f1 <= countfile.output; f1 += 1) {
#	    pickfile (t_inlist, f1)
#	    img = pickfile.output

	    # Strip off extension of input file name.
	    unlearn fparse
	    fparse (img, verbose-)
	    simg = fparse.directory // fparse.root
#	    if (t_verb) 
	    print ("Working on ", img, "..." )

	    # If extension is empty, use imtype.
	    if (strlen (fparse.extension) == 0)
	        fparse.extension = imtp

	    # Main deriv loop:
	    convolve.bilinear = yes
	    outimg = simg // "_obj" // fparse.extension

            if ((substr (fparse.extension, 2, 4) == "fit") ||
                (substr (fparse.extension, 2, 4) == "imh"))
	        t_ngroup = 1

            if (substr (fparse.extension, 2, 4) == "fit") {
	        imcopy (fparse.root // fparse.extension // "[0]", outimg, 
                        verb=t_verb)
	        imcopy (img, outimg // "[APPEND]", verb=t_verb)
            } else
	        gcopy  (img, outimg, groups="ALL", verb=t_verb)

            if (substr (fparse.extension, 2, 4) == "imh")
	        hedit (outimg, "GCOUNT", "",add-,del+,verify-, > "dev$null")

            if (substr (fparse.extension, 2, 4) == "fit") {
	        if (t_verb)
	            hedit (outimg // "[1]", "ORG_TITL", img, add+, verify-)
	        else
	            hedit (outimg // "[1]", "ORG_TITL", img, add+, verify-, > "dev$null")
	    } else {
	        if (t_verb)
	            hedit (outimg, "ORG_TITL", img, add+, verify-)
	        else
	            hedit (outimg, "ORG_TITL", img, add+, verify-,>"dev$null")
	    }

	    imcals = "if (im1 .lt. 70) then im1 else sqrt(abs(im1 - 70))+70"
	    imcals = "if (im1 .lt. 0) then 0 else sqrt(im1)"
	    if (t_dosky) {
                if (substr (fparse.extension, 2, 4) == "fit")
	            sky (outimg // "[1]", subsky+, skyname=t_skyn, verb=t_verb)
	        else
	            sky (outimg, subsky+, skyname=t_skyn, verb=t_verb)
	    }

	    for (ii = 1; ii <= t_ngroup; ii += 1) {

	        # Build temporary file names.
	        holdfile = mktemp (t_tempdir // "pre_cor") // "." // imtp
	        hold     = mktemp (t_tempdir // "pre_cor") // "." // imtp
	        hold1    = mktemp (t_tempdir // "pre_cor") // "." // imtp
	        hold2    = mktemp (t_tempdir // "pre_cor") // "." // imtp

	        holdfile = mktemp (t_tempdir // "pre_cor") // imtp
	        hold     = mktemp (t_tempdir // "pre_cor") // imtp
	        hold1    = mktemp (t_tempdir // "pre_cor") // imtp
	        hold2    = mktemp (t_tempdir // "pre_cor") // imtp

	        if (substr (fparse.extension, 2, 4) == "fit") {
	            in_name  = outimg // "[1]"
	            out_name = outimg
	        } else if (substr (fparse.extension, 2, 4) == "imh") {
	            in_name  = outimg
	            out_name = outimg
	        } else {
	            in_name  = outimg // "[" // ii // "]"
	            out_name = outimg // "[" // ii // "]"
	        }

	        imcopy (in_name, holdfile, verb=t_verb)
	        imstr = "if (im1 .ge. " // t_minval // ") then 1 else 0"
	        imcalc (holdfile, hold, imstr, verb=t_verb)

	        convolve (hold, hold, "", find_kern, find_kern)
	        imstr = "if (im1 .ge. " // t_minpix // ") then 1 else 0"
	        imcalc (hold, hold1, imstr, verb=t_verb)

	        convolve (hold1, hold1, "", save_kern, save_kern)
	        imstr = "if (im1 .gt. 0) then 1 else 0"
	        imcalc (hold1, hold2, imstr, verbose=t_verb)

	        imarith (hold2, "*", holdfile, holdfile, verb=t_verb)
	        if (t_dosqrt) {
	            if ((substr (fparse.extension, 2, 4) == "fit") ||
	                (substr (fparse.extension, 2, 4) == "imh") )
	                imdelete (out_name, ver-,  > "dev$null")
	            imcalc (holdfile, out_name, imcals, verb=t_verb)
	        } else {
	            if (substr (fparse.extension, 2, 4) == "fit") {
	                imdelete (out_name, ver-,  > "dev$null")
	                imcopy (holdfile, out_name, verb=t_verb)
	                hedit (out_name, "ORG_TITL", img, add+, verify-, > "dev$null")
	            } else {
	                if (substr (fparse.extension, 2, 4) == "imh") {
                            imdelete (out_name, ver-, >& "dev$null")
	                    imcopy (holdfile, out_name, verb=t_verb) 
                        } else
	                    imcopy (holdfile, out_name, verb=t_verb) 
	            }
	        }

	        # Delete temporaries.
	        imdel (holdfile, verify-, > "dev$null")
	        imdel (hold,     verify-, > "dev$null")
	        imdel (hold1,    verify-, > "dev$null")
	        imdel (hold2,    verify-, > "dev$null")
	    }  
	}

	delete (t_list, verify-, > "dev$null")
	filelist = ""
end

