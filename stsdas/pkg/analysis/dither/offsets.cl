#
# OFFSETS  --  Runs crossdriz on 4-group WFPC images.
#
#
#
#  13-Jun-96:  Task created (I. Busko)
#  17-Jun-96:  User-settable output crosscor basename.
#  26-Jun-96:  Add cdrip pset.
#  07-Oct-96:  Replace drip by drizzle.
#  07-Nov-96:  File list support.
#  21-Mar-97:  Header-type coefficients spec.
#  09-Apr-97:  Output is file root name or list/template.
#  07-Jul-97:  Revise calls that rely on the 'imtype' setting (IB)
#  07-Jul-97:  Suppress suffix when proocessing just one file (IB)
#  14-Nov-97:  Replace "cubic" and "trauger" by "header" (IB)
#  29-May-98:  Put back rotation angle in cdriz (IB)
#  14-Aug-98:  Pass original image info to later dither stages (A. Fruchter)


procedure offsets (input, refimage, output)

char	input     = ""      {prompt="List with input image names"}
char	refimage  = ""      {prompt="Reference image"}
char	output    = ""      {prompt="List or rootname for crosscor images"}
char	coeffs    = "header"{prompt="Coefficient type"}
pset	cdriz               {prompt="CROSSDRIZ parameters"}
file	tempdir   = "tmp$"  {prompt="Directory for temporary files"}
bool	verbose   = yes     {prompt="Print info ?"}

struct	*list1              {prompt="Internal parameter, do not use."}
struct	*list2              {prompt="Internal parameter, do not use."}

char    version  = "13Jul2000" {prompt="Date of installation"}

begin
	file 	inp, out, ref, infile, outfile, t_list1, t_list2
	file	inp_group, ref_group, tempfile, ginp, gout, g_list1, g_list2
	char	msg, coef, cam, dcoef
	int	group, gcount, fcount, smargin, staper, f1
	real	rot, smint, smaxt, sstep, sxout, syout
	bool    verb, sdinp, sdref, sverb, spad
        char    title
        int     org_group
	real    org_exp

	# Check for the presence of pre-requisite tasks and packages.
	msg = ""
	if (!deftask("hselect"))   msg = msg // " images"
	if (!deftask("crosscor"))  msg = msg // " fourier"
	if (strlen(msg) > 0) {
	    printf ("Please, load/define: %s\n", msg)
	    bye
	}

	# Read task parameters.
	inp    = input
	ref    = refimage
	out    = output
	coef   = coeffs
	rot    = cdriz.rotation
	verb   = verbose

	# Save crossdriz parameters.
	sdinp   = crossdriz.dinp
	sdref   = crossdriz.dref
	smargin = crossdriz.margin
	staper  = crossdriz.tapersz
	smint   = crossdriz.mintheta
	smaxt   = crossdriz.maxtheta
	sstep   = crossdriz.stptheta
	sxout   = crossdriz.xout
	syout   = crossdriz.yout
	sverb   = crossdriz.verbose
	spad    = crossdriz.pad

	# Test consistency of coefficient file names.
	if (strlen (out) == 0)
	    error (0, "No output basename.")
#	if ((coef != "cubic")   &&
#	    (coef != "trauger") &&
#	    (coef != "header") &&
#	    (coef != ""))
#	    error (0, "Invalid coefficient file specification.")
	if ((coef != "header")   &&
	    (coef != ""))
	    error (0, "Invalid coefficient file specification.")

	# Create temporary image names.
	tempfile  = tempdir // "dith" 
	inp_group = mktemp (tempfile)
	#ref_group = mktemp (tempfile) // ".hhh"
	ref_group = mktemp (tempfile) 
	t_list1   = mktemp (tempfile)
	t_list2   = mktemp (tempfile)
	g_list1   = mktemp (tempfile)
	g_list2   = mktemp (tempfile)

	# Expand file lists. Input is always a template or list, but
	# output may be a template/list or a rootname. In the later
	# case, assemble the list using _XX sufixes.
	files (inp, sort-, > t_list1)
	countfile (out)
	if (countfile.output > 1)
	    files (out, sort-, > t_list2)
	else {
	    countfile (inp)
	    if (countfile.output > 1) {
	        for (f1 = 1; f1 <= countfile.output; f1+=1) {
	            if (f1 < 10)
	                print (out // "_0" // str(f1), >> t_list2)
	            else 
	                print (out // "_"  // str(f1), >> t_list2)
	        }
	    } else {
	        print (out, > t_list2)
	    }
	}

	# Check that input and output lists have the same size.
	list1 = t_list1
	list2 = t_list2
	while (fscan (list1, ginp) != EOF) {
	    if (fscan (list2, gout) == EOF)
	    error (0, "Output list is shorter than input.")
	}
	list1 = t_list1
	list2 = t_list2
	while (fscan (list2, ginp) != EOF) {
	    if (fscan (list1, gout) == EOF)
	    error (0, "Output list is longer than input.")
	}

	# Check group structure of reference image.
	hselect (ref, "gcount", "yes") | scan (gcount)
	if (gcount != 4)
	    error (1, "Unexpected number of groups in reference image.")

	# Set CROSSDRIZ parameters.
	crossdriz.dinp     = no
	if ((strlen(coef) > 0) || (rot > 0.0))
	    crossdriz.dinp = yes
	if (strlen(coef) > 0)
	    crossdriz.dref = yes
	else
	    crossdriz.dref = no
	crossdriz.margin   = cdriz.margin
	crossdriz.tapersz  = cdriz.tapersz
	crossdriz.mintheta = rot
	crossdriz.maxtheta = rot
	crossdriz.stptheta = 1.0
	crossdriz.xout     = INDEF
	crossdriz.yout     = INDEF
	crossdriz.verbose  = verb
	crossdriz.pad      = cdriz.pad

	# Main loop. Process each group of 4-group WFPC images.
	for (group = 1;  group <= 4;  group += 1) {

	    # First, extract current group from reference image.
	    if (verb)
	        printf ("Extracting group %d of reference image\n", group);
	    #gcopy (ref, ref_group, groups=group, i2toi4-, verbose=verb)
	    imcopy (ref//"["//str(group)//"]", ref_group, verbose-)

	    # Now scan file lists and process each in turn.
	    list1  = t_list1
	    list2  = t_list2
	    fcount = 1
	    while (fscan (list1, ginp) != EOF) {
	        i = fscan (list2, gout)

	        # Check group structure of current input file.
	        hselect (ginp, "gcount", "yes") | scan (gcount)
	        if (gcount != 4)
	            error (1, "Unexpected number of groups in input file.")

	        # Add sequential suffix to generate input name.
	        ##infile = inp_group // str(fcount) // ".hhh"
	        infile = inp_group // str(fcount) 
	        fcount += 1

	        # Extract current group from current image.
	        if (verb)
	            printf ("Extracting group %d of %s\n", group, ginp);
	        #gcopy (ginp, infile, groups=group, i2toi4-, verbose=verb)
	        imcopy (ginp//"["//str(group)//"]", infile, verbose-)

	        # Add name to input list for CROSSDRIZ.
	        print (infile, >> g_list1)

                # ADD ORIGINAL IMAGE INFO TO HEADER
                imgets(ginp, "ORG_TITL", mode="h")
                title = imgets.value
                fparse(ginp)
                if (title == "0")  {
                   title = fparse.root//fparse.extension
                }
		imgets(ginp,"exptime",mode="h")
		org_exp = real(imgets.value)
                hedit (infile, "ORG_TITL", title, add+, show-, update+, verify-)
                hedit (infile, "ORG_TITL", title, add+, show-, update+, verify-)
                hedit (infile, "ORG_GRP", group, add+, show-, update+, verify-)
                hedit (infile, "ORG_GRP", group, add+, show-, update+, verify-)
		print ("OFFSETS: ",title," ",group)

	        # Add group-related suffix to generate output name.
	        outfile = gout
	        outfile = outfile // "_g" // str(group)

	        # Add name to output list for CROSSDRIZ.
	        print (outfile, >> g_list2)
	    }

	    # Build drizzle coeff file name.
	    if (strlen(coef) > 0) {
	        if (group == 1)
	            cam = "pc"
	        else
	            cam = "wf"
	        if (coef == "header")
	            dcoef = coef
	        else
	            dcoef="drizzle$coeffs/" //cam // str(group) // "-" // coef
	        if (verb)
	            print ("Drizzle coefficients taken from ", dcoef)
	    } else {
	        dcoef = ""
	        if (verb)
	            print ("No geometrical correction done.")
	    }

	    # Drizzle and cross-correlate.
	    crossdriz ("@" // g_list1, ref_group, "@" // g_list2, \
                       coeffs=dcoef, dummy=yes)

	    # Delete current group's inputs and reference.
	    imdelete ("@" // g_list1, verify-, >& "dev$null")
	    imdelete (ref_group, verify-, >& "dev$null")

	    # Delete CROSSDRIZ file lists for current group.
	    delete (g_list1, verify-, >& "dev$null")
	    delete (g_list2, verify-, >& "dev$null")
            flprcache

	}

	# Recover crossdriz parameters.
	crossdriz.dinp     = sdinp
	crossdriz.dref     = sdref
	crossdriz.margin   = smargin
	crossdriz.tapersz  = staper
	crossdriz.mintheta = smint
	crossdriz.maxtheta = smaxt
	crossdriz.stptheta = sstep
	crossdriz.xout     = sxout
	crossdriz.yout     = syout
	crossdriz.verbose  = sverb
	crossdriz.pad      = spad

	delete   (t_list1, verify-, >& "dev$null")
	delete   (t_list2, verify-, >& "dev$null")

	list1 = ""
	list2 = ""

	print ("*** OFFSETS done ***")
end
