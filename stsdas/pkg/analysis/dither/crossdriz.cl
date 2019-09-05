#
# CROSSDRIZ  --  Creates family of cross-correlation images from 
#                one input image, one reference image, and a set 
#                of rotation angles.
#
#  Input and reference images must be single-group only.
#
#
#  18-Nov-95:  Task created (I. Busko)
#  25-Dec-95:  Rotate replaced by drip (A. Fruchter)
#  10-Apr-96:  STSDAS version (IB)
#  03-Jun-96:  Optional drizling of reference image (IB)
#  25-Jul-96:  Variable output image size and margin instead of section (IB)
#  07-Oct-96:  Drip replaced by drizzle (IB)
#  07-Nov-96:  Support for file list processing (IB)
#  07-Nov-96:  Debugging control to skip drizzle and crosscor tasks (IB)
#  02-Jan-97:  Fixed section string construction (IB)
#  14-Jan-97:  Replaced "files" by "pickfile/countfile" in list expansion (IB)
#  28-Jan-97:  Use either parameter list or 'cdriz' pset (IB)
#  07-Jul-97:  Revise calls that rely on the 'imtype' setting (IB)
#  08-Sep-97:  Add 'imtype' value as extension in temporary images (IB)
#  27-Oct-97:  Ignore coefficients from header if not WFPC file (IB)
#  05-Mar-98:  Remove exp_sc drizzle parameter (IB)
#  16-Apr-98:  Add expkey drizzle parameter (IB)
#  14-Jul-98:  Set pixfrac parameter (IB)
#  14-Aug-98:  Pass original image info to later stages  (ASF & RH)
#  01-Oct-98:  Set xsh,ysh = 0 explictly in drizzle calls (IB)
#  20-Oct-98:  Read coeffs from header if it is WFPC (see 27-Oct-97, IB)
#  23-Jun-00:  Read coeffs from header for STIS and NICMOS as well as WFPC2

procedure crossdriz (image, refimage, basename)

char	image    = ""      {prompt="Input image list"}
char	refimage = ""      {prompt="Reference image"}
char	basename = ""      {prompt="Paired basename list for crosscor images"}
bool    dinp     = yes     {prompt="Drizzle input image(s) ?"}
bool    dref     = yes     {prompt="Drizzle reference image ?"}
int	margin   = 50      {min=0,prompt="Margin to strip down"}
int	tapersz  = 50      {min=1, prompt="Edge region to taper"}
real	mintheta =  0.     {prompt="Minimum rotation angle"}
real	maxtheta =  0.     {prompt="Maximum rotation angle"}
real	stptheta =  0.1    {min=1.E-5, prompt="Angle step"}
char	coeffs   = "header" {prompt="Drizzle coefficients file name"} 
real	lambda   = 555.0   {prompt="Effective Wavelength (nm), Trauger coefficients only"}
char    expkey   = "exptime" {prompt="Exposure time keyword in input data image header"}
real	xout     = INDEF   {prompt="Drizzled image size"}
real	yout     = INDEF
bool	pad      = yes      {prompt="Pad image to prevent wraparound effects ?"}
file	tempdir  = "tmp$"  {prompt="Directory for temporary files"}
bool	verbose  = yes     {prompt="Print info ?"}
bool	dummy    = no      {prompt="Internal parameter, do not use."}
char    version  = "13Jul2000" {prompt="Date of installation"}

begin
	# These hold input parameters.
	file	ima, ref, root, imain, imaout
	char	sec
	int	tsz, mrg
	real	mina, maxa, step, ox, oy
	bool	verb, tdref, tdinp, tpad

	# These are for internal use.
	file	t_rot, t_taper, t_ref, t_temp
	file    t_junk, t1_ref, tempfile
	char    msg, imtp
	int	name_index, xszi, yszi, xszr, yszr 
	int	f1, f2
	real	angle
	bool	dodriz, dogeom, dorot
	bool	debug

	# These are for saving drizzle parameters.
	char	scoeffs, soutweig, sinmask, swtscl, sexpkey
	int	soutnx, soutny
	real	sscale, slambda, srot, spixf
	char    title
	int     group
        int     org_grp
        bool    title_up, group_up
        real    org_exp

	# Set this to "yes" to replace drizzle and crosscor by
	# much faster imcopy. Useful for testing housekeeping
	# functionality only.
	debug = no
	if (debug) print ("*** RUNNING IN DEBUG MODE. ***") 

	# Check for the presence of pre-requisite tasks/packages.
	msg = ""
	if (!deftask("rotate"))   msg = msg // " images"
	if (!deftask("crosscor")) msg = msg // " fourier"
	if (!deftask("pickfile")) msg = msg // " imgtools"
	if (strlen(msg) > 0) {
	    printf ("Please, load packages: %s\n", msg)
	    bye
	}

	# Read task parameters.
	ima   = image
	ref   = refimage
	root  = basename
	mina  = mintheta
	maxa  = maxtheta
	step  = stptheta
	tdref = dref
	tdinp = dinp
	ox    = xout
	oy    = xout
	verb  = verbose

	# This is to enable reading either from the 'cdriz' pset 
	# or the main parameter list. When executed from the 
	# command line, parameters will be read from the parameter
	# list; when called from "offsets", parameters will be read 
	# from the 'cdriz' pset instead.
	if (dummy) {
	    tpad = cdriz.pad
	    mrg  = cdriz.margin
	    tsz  = cdriz.tapersz
	} else {
	    tpad = pad
	    mrg  = margin
	    tsz  = tapersz
	}

	# Save drizzle parameters. Coeffs is set only when image
        # header becomes available so instrument can be known.
	scoeffs  = drizzle.coeffs
	sscale   = drizzle.scale
	slambda  = drizzle.lambda
	soutweig = drizzle.outweig
	sinmask  = drizzle.in_mask
	soutnx   = drizzle.outnx
	soutny   = drizzle.outny
	srot     = drizzle.rot
	swtscl   = drizzle.wt_scl
	sexpkey  = drizzle.expkey
	spixf    = drizzle.pixfrac

	# Set drizzle parameters.
	drizzle.coeffs  = coeffs
	drizzle.scale   = 1.0
	drizzle.lambda  = lambda 
	drizzle.outweig = ""
	drizzle.in_mask = ""
	drizzle.wt_scl  = "1."
	drizzle.expkey  = expkey
	drizzle.pixfrac = 1.0

	if ((maxa - mina) < 0.) error (0, "maxtheta < mintheta.")

	# Create temporary image names.
	tempfile = tempdir // "dith"
	show ("imtype") | scan (imtp)
	t_taper  = mktemp (tempfile) // "." // imtp
	t_ref    = mktemp (tempfile) // "." // imtp
	t1_ref   = mktemp (tempfile) // "." // imtp
	t_rot    = mktemp (tempfile) // "." // imtp

	# Check that input and output file lists have the same size.
	countfile (ima)
	f1 = countfile.output
	countfile (root)
	f2 = countfile.output
	if (f1 != f2)
	    error (0, "Input and output lists have different sizes.")

	# Set flags that tell if there is geometrical correction or rotation.
	if ((strlen(coeffs) != 0) ||
            (ox != INDEF)         ||
            (oy != INDEF))
	    dogeom = yes
	else
	    dogeom = no
	if ((mina != 0.0) || (maxa != 0.0)) {
	    dorot = yes
	    if (!tdinp)
	        error (0, "Must drizzle input in order to rotate.")
	} else
	    dorot = no

	if (verb) print ("*** Starting CROSSDRIZ ***")

	# Processing of reference image. First, set the
	# proper output size for DRIZZLE.
	imgets (ref, "i_naxis1", mode="h")
	xszr = int(imgets.value)
	imgets (ref, "i_naxis2", mode="h")
	yszr = int(imgets.value)
	drizzle.outnx = xszr
	drizzle.outny = yszr
	if (ox != INDEF && oy != INDEF && tdref) {
	    drizzle.outnx = ox
	    drizzle.outny = oy
	}

	# Build section spec to be used by imcopy.
	sec = "[" // str(mrg+1) // ":" // str(drizzle.outnx-mrg) // ","
	sec = sec // str(mrg+1) // ":" // str(drizzle.outny-mrg) // "]"

	# Explicit drizzle reference only if asked for AND there is 
	# geometrical correction.
	if (tdref && dogeom) {
	    if (verb) print ("Drizzling reference...")
	    if (!debug) {
	        if (coeffs == "header") {
	            imgets (ref, "instrume", mode="h")
	            if (imgets.value != "WFPC2" && \
                        imgets.value != "STIS" && \
                        imgets.value != "NICMOS") {
	                drizzle.coeffs = ""
	            print ("WARNING: No coefficients from header are allowed.")
	            }
	        }
	        drizzle (ref, t1_ref, rot=0., xsh=0., ysh=0.)
	    } else
	        imcopy (ref, t1_ref, verbose-)
	    if (verb) print ("Done drizzling reference.")
	    imcopy (t1_ref // sec, t_ref, verbose=verb)
	} else {
	    imcopy (ref // sec, t_ref, verbose=verb)
	    if (dogeom)
	        print ("WARNING: Not drizzling reference image.")
	}
	if (verb) print ("Tapering...")
	taperedge (t_ref, t_ref, width=tsz, subtract="edge", \
	           function="cosbell", verbose=verb, mode="h")

	# Main loop. Scan file lists and process each in turn.
	countfile (ima)
        for (f1 = 1; f1 <= countfile.output; f1+=1) {
	    pickfile (ima, f1)
	    imain = pickfile.output
	    pickfile (root, f1)
	    imaout = pickfile.output

	    # Check input image sizes.
	    imgets (imain, "i_naxis1", mode="h")
	    xszi = int(imgets.value)
	    imgets (imain, "i_naxis2", mode="h")
	    yszi = int(imgets.value)
	    if ((xszi != xszr) || (yszi != yszr))
	        error (0, "Image and reference have different sizes.")

       #XDITHER: Extracting original image name and group
	    imgets(imain, "ORG_TITL", mode="h")
	    title = imgets.value
	    fparse(imain)
	    if (title == "0")  {
	       title = fparse.root//fparse.extension
               title_up = yes
	    } 

            imgets(imain,"ORG_GRP", mode="h")
            group = int(imgets.value)
            fparse(imain)

	    if (imgets.value == "0") {
		print ("No ORG_GRP found")
	        group = fparse.cl_index
		print ("group is ",group)

	       if (group == -1) {
	          if (fparse.extension == ".fits" || fparse.extension == ".fit")
		      org_grp = 0
	 
   	          if (fparse.extension == ".imh" || fparse.extension == ".hhh")
		      org_grp = 1

                  if (fparse.extension == "")
                      org_grp = 0
	       } else {org_grp = group}
	    } else {org_grp = group}

#	    print ("CROSSDRIZ group value,",title, org_grp)

            imgets(imain,expkey, mode="h")
	    org_exp = real(imgets.value)

	    # Process input images through all steps: drizzle, copy section, 
	    # taper, cross correlate. Angle is written to crosscorr image 
	    # header.
	    name_index = 1
            for (angle = mina; angle <= maxa; angle=angle+step) {

	        # Check that angle is reasonable.
	        if (abs(angle) < 1.E-5) angle = 0.0

	        # Drizzle, but only if there is geom/rot correction. 
	        # Otherwise just copy.
	        if ((dogeom || dorot) && tdinp) {
	           if (verb) 
	                print ("Drizzling with ",angle," degree rotation...")
	            drizzle.rot = angle
	            if (!debug) {
	                if (coeffs == "header") {
	                    imgets (imain, "instrume", mode="h")
	                    if (imgets.value != "WFPC2" && \
                                imgets.value != "STIS" && \
                                imgets.value != "NICMOS") {
	                        drizzle.coeffs = ""
	            print ("WARNING: No coefficients from header are allowed.")
	                    }
	                }
	                drizzle (imain, t_rot, xsh=0., ysh=0.)
	            } else
	                imcopy (imain, t_rot, verbose-)
	            dodriz = yes
	        } else {
	            t_rot = imain
	            dodriz = no
	            if (dogeom || dorot)
	                print ("WARNING: Not drizzling image.")
	        }

	        # Copy section.
	        if (verb && dodriz) 
	            print ("Done drizzling. Now copying section...")
	        imcopy (t_rot // sec, t_taper, verbose=verb)
	        if (dodriz) 
                    imdelete (t_rot, verify-, >& "dev$null")

	        # Taper edge.
	        if (verb) print ("Tapering...")
	        taperedge (t_taper, t_taper, width=tsz, subtract="edge", \
	                   function="cosbell", verbose=verb, mode="h")

	        # Cross-correlate. If only one rotated image, do not put suffix.
	        if (verb) print ("Cross-correlating...")
	        if (maxa != mina) {
	            if (name_index < 10)
	                t_temp = imaout // "_0" // name_index
	            else  
	                t_temp = imaout // "_"  // name_index
	        } else 
	            t_temp = imaout

	        if (!debug)
	            crosscor (t_ref, t_taper, t_temp, pad=tpad, verbose=verb)
	        else
	            imcopy (t_ref, t_temp, verbose-)

	        # Update image header with angle.
	        hedit (t_temp, "ROTANGLE", angle, add+, show-, update+, verify-)
		print ("CROSSDRIZ updating title: ",t_temp," ",title)
	        hedit (t_temp, "ORG_TITL", title, add+, show-, update+, verify-)
	        hedit (t_temp, "ORG_TITL", title, add+, show-, update+, verify-)
#		print ("CROSSDRIZ updating group: ",t_temp," ",org_grp)

	        hedit (t_temp, "ORG_GRP", org_grp, add+, show-, update+, verify-)
	        hedit (t_temp, "ORG_GRP", org_grp, add+, show-, update+, verify-)
	        hedit (t_temp, "ORG_EXP", org_exp, add+, show-, update+, verify-)
	        hedit (t_temp, "ORG_EXP", org_exp, add+, show-, update+, verify-)

	        # Prepare for next angle.
	        name_index = name_index + 1
	        imdelete (t_taper, verify-, >& "dev$null")
	    }
	}

	# Recover drizzle parameters.
	drizzle.coeffs  = scoeffs
	drizzle.scale   = sscale 
	drizzle.lambda  = slambda
	drizzle.outweig = soutweig 
	drizzle.in_mask = sinmask
	drizzle.outnx   = soutnx 
	drizzle.outny   = soutny 
	drizzle.rot     = srot 
	drizzle.wt_scl  = swtscl 
	drizzle.expkey  = sexpkey 
	drizzle.pixfrac = spixf

	imdelete (t1_ref,  verify-, >& "dev$null")
	imdelete (t_ref,   verify-, >& "dev$null")

	if (verb) print ("*** CROSSDRIZ finished ***")
end
