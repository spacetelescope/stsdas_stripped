#
# LOOP_BLOT  --  Runs blot for a given list of input, output images
#
#
#
#  23-Jun-00:  Task created (A. Koekemoer)
#
#


procedure loop_blot (input, output, shiftfile)

char	input       = ""      {prompt="Single filename or filelist for input image(s)"}
char	output      = ""      {prompt="Filelist with output image names"}
file	shiftfile   = "average_shifts"      {prompt="File containing x,y shifts for each image"}
int	group       = "2"     {prompt="Group number (for reading the shiftfile)"}
char	shifttask   = "avshift" {prompt="Task which produced the shiftfile",enum="avshift|shiftfind"}
char	blot_suffix = "_bl  " {prompt="Suffix for output blotted images"}
file	tempdir     = "tmp$"  {prompt="Directory for temporary files"}
bool	verbose     = yes     {prompt="Print info ?"}

struct	*list1              {prompt="Internal parameter, do not use."}
struct	*list2              {prompt="Internal parameter, do not use."}
struct	*list4              {prompt="Internal parameter, do not use."}

char    version  = "23Jun00" {prompt="Date of installation"}

begin
	file 	inp, out, infile, outfile, fileroot
	file	inp_blot, out_blot, ext
	file	tempfile, t_list1, t_list2
	char	msg, str_img, str_grp, str1, str2, str3, str4, instrument
	int	gcount, f1, n_inp, n_out
	real	xsh, ysh
	bool    verb, found_img

	# Check for the presence of pre-requisite tasks and packages.
	msg = ""
	if (!deftask("hselect"))   msg = msg // " images"
	if (!deftask("gcopy"))     msg = msg // " imgtools"
	if (strlen(msg) > 0) {
	    printf ("Please, load/define: %s\n", msg)
	    bye
	}

	# Read task parameters.
	inp    = input
	out    = output
	verb   = verbose


	# Create temporary image names.
	tempfile  = tempdir // "dith" 
	t_list1   = mktemp (tempfile)
	t_list2   = mktemp (tempfile)

	# Count how many files are in each of the 3 input lists ("one or many?")
	countfile(inp)
	n_inp = countfile.output
	countfile(out)
	n_out = countfile.output


	# Expand file lists. Output must always be a filelist, but
	# input file may be either a filelist or a single file.

	files (out, sort-, > t_list2)

	if (n_inp > 1) {
	    files (inp, sort-, > t_list1)
	} else {
	    for (f1 = 1; f1 <= n_out; f1+=1) print (inp, >> t_list1)
	}


	# Check that input file list is at least as long as output list.
	# Note - they should always be the same size, if the above loop has
	# done what it is supposed to do. So the following should never occur.

	countfile("@"//t_list1)
	if (countfile.output != n_out) error (0, "Input and output lists are different sizes")



	# Main loop.

	# Scan file lists and process each in turn.
	list1  = t_list1
	list2  = t_list2
	while (fscan (list1, infile) != EOF) {

	    i = fscan (list2, outfile)

	    # Extract the output filename root (ie remove directory path and
	    # any extension that may be present)
	    fparse (outfile)
	    fileroot = fparse.root

	    # Set up the instrument -- "group" will only be used for WFPC2, and
	    # ignored otherwise.
	    imgets (infile, "instrume", mode="h")
            instrument = imgets.value

	    # Read in the xsh, ysh values from shiftfile, selecting them
	    # according to the current input image "infile" and group number.

	    list4 = shiftfile
	    while (fscan (list4,str_img,str_grp,str1,str2,str3,str4) != EOF) {

	        # First, see if "fileroot" is at all present in "str_img":
	        found_img = no
	        k = strlen(fileroot)
	        for (i = 1; i <= strlen(str_img)-k+1; i+=1) {
	            if (substr(str_img, i, i+k-1) == fileroot) found_img = yes
	        }

	        if (instrument == "WFPC2") {
	            if (found_img && (str_grp == str(group))) {
	                if (shifttask == "avshift") {
	                    print(str3) | scan(xsh)
	                    print(str4) | scan(ysh)
	                } else {
 	                    print(str1) | scan(xsh)
	                    print(str3) | scan(ysh)
	                }
	            }
	        } else {
	            if (found_img) {
	                if (shifttask == "avshift") {
	                    print(str3) | scan(xsh)
	                    print(str4) | scan(ysh)
	                } else {
 	                    print(str1) | scan(xsh)
	                    print(str3) | scan(ysh)
	                }
	            }
	        }
	    }


	    # Set up the output blotted filename
	    # and add the appropriate extension again as well.
	    fparse (outfile)
	    fileroot = fparse.root
	    ext = fparse.extension

	    out_blot = fileroot // blot_suffix // ext


	    # Check group structure of current input file "infile".
	    # If it's a 4-group file, then copy the group that is
	    # specified in the parameter called "group".
	    # If it's a single-group file then just use it as-is.

	    hselect (infile, "gcount", "yes") | scan (gcount)
	    if (gcount == 4) {
	        inp_blot = mktemp (tempfile)
	        printf("\ngcopy %s[%s] %s\n", infile, str(group), inp_blot)
	        gcopy (infile, inp_blot, groups=group, i2toi4-, verbose-)
	    } else {
	        #printf("\nimcopy %s %s\n", infile, inp_blot)
	        #imcopy (infile, inp_blot, verbose-)
	        printf("\n")
	        inp_blot = infile
	    }


	    # Call Blot.

 	    printf ("blot %s %s xsh=%s ysh=%s\n\n", \
	             inp_blot, out_blot, str(xsh), str(ysh))
	    blot (inp_blot, out_blot, xsh=xsh, ysh=ysh)

	    # Remove the temporary input file (only if it was created in the
	    # first place!
	    if (gcount == 4) imdelete (inp_blot, verify-, >& "dev$null")

            flprcache

	}


	delete   (t_list1, verify-, >& "dev$null")
	delete   (t_list2, verify-, >& "dev$null")

	list1 = ""
	list2 = ""

	printf ("\n*** LOOP_BLOT done ***\n")
end
