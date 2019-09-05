#
# LOOP_DRIZ  --  Runs drizzle for a given list of input, output images
#
#
#
#  23-Jun-00:  Task created (A. Koekemoer)
#
#


procedure loop_driz (input, output, masks, shiftfile)

char	input       = ""      {prompt="Filelist with input image names"}
char	output      = ""      {prompt="Single filename or filelist for output image(s)"}
char	masks       = ""      {prompt="Single filename, or filelist, for mask image(s)"}
file	shiftfile   = "average_shifts"      {prompt="File containing x,y shifts for each image"}
int	group       = "2"     {prompt="Group number (for reading the shiftfile)"}
char	shifttask   = "avshift" {prompt="Task which produced the shiftfile",enum="avshift|shiftfind"}
char	driz_suffix = "_dr"   {prompt="Suffix for output drizzled images"}
char	wght_suffix = "_drw"  {prompt="Suffix for output weight images"}
file	tempdir     = "tmp$"  {prompt="Directory for temporary files"}
bool	verbose     = yes     {prompt="Print info ?"}

struct	*list1              {prompt="Internal parameter, do not use."}
struct	*list2              {prompt="Internal parameter, do not use."}
struct	*list3              {prompt="Internal parameter, do not use."}
struct	*list4              {prompt="Internal parameter, do not use."}

char    version  = "23Jun00" {prompt="Date of installation"}

begin
	file 	inp, out, mask, infile, outfile, maskfile, fileroot
	file	inp_driz, out_driz, out_wght, ext
	file	tempfile, t_list1, t_list2, t_list3
	char	msg, str_img, str_grp, str1, str2, str3, str4, instrument
	int	gcount, f1, i, j, k, n_inp, n_out, n_mask
	real	xsh, ysh
	bool    verb, found_img, temp_file
	# This is for saving drizzle parameters:
	char	scoeffs


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
	mask   = masks
	verb   = verbose

	scoeffs  = drizzle.coeffs



	# Create temporary image names.
	tempfile  = tempdir // "dith" 
	t_list1   = mktemp (tempfile)
	t_list2   = mktemp (tempfile)
	t_list3   = mktemp (tempfile)

	# Count how many files are in each of the 3 input lists ("one or many?")
	countfile(inp)
	n_inp = countfile.output
	countfile(out)
	n_out = countfile.output
	countfile(mask)
	n_mask = countfile.output

	# Take care of the case where mask may be specified as "":
	if (mask == "") n_mask = 1


	# Expand file lists. Input can be a template or a filelist, and
	# output and mask file may be either a filelist or a single file.

	files (inp, sort-, > t_list1)

	if (n_out > 1) {
	    files (out, sort-, > t_list2)
	} else {
	    for (f1 = 1; f1 <= n_inp; f1+=1) printf ("%s\n",out, >> t_list2)
	}


	if (n_mask > 1) {
	    files (mask, sort-, > t_list3)
	} else {
	    for (f1 = 1; f1 <= n_inp; f1+=1) printf ("%s\n",mask, >> t_list3)
	}

	# Check that output and mask lists are at least as long as input list.
	# Note - they should always be the same size, if the above loop has
	# done what it is supposed to do. So the following should never occur.
	#
	#countfile("@"//t_list2)
	#if (countfile.output != n_inp) error (0, "Input and output lists are different sizes")
	#countfile("@"//t_list3)
	#if (countfile.output != n_inp) error (0, "Input and mask lists are different sizes")



	# Main loop.

	# Scan file lists and process each in turn.
	list1  = t_list1
	list2  = t_list2
	list3  = t_list3
	while (fscan (list1, infile) != EOF) {

	    j = fscan (list2, outfile)
	    j = fscan (list3, maskfile)

	    # Extract the input filename root (ie remove directory path and
	    # any extension that may be present)
	    fparse (infile)
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


	    # Set up the output drizzle and weight filenames
	    # and add the appropriate extension as well.

	    fparse (outfile)
	    fileroot = fparse.root
	    ext = fparse.extension

	    out_driz = fileroot // driz_suffix // ext
	    out_wght = fileroot // wght_suffix // ext


	    # Check group structure of current input file "infile".
	    # If it's a 4-group file, then copy the group that is
	    # specified in the parameter called "group".
	    # If it's a single-group file then just use it as-is.

	    hselect (infile, "gcount", "yes") | scan (gcount)
	    if (gcount == 4) {
	        inp_driz = mktemp (tempfile)
	        #printf("\ngcopy %s[%s] %s\n", infile, str(group), inp_driz)
	        #gcopy (infile, inp_driz, groups=group, i2toi4-, verbose-)
	        printf("\nimcopy %s[%s] %s\n", infile, str(group), inp_driz)
	        imcopy (infile//"["//str(group)//"]", inp_driz, verbose-)
	    } else {
	        #printf("\nimcopy %s %s\n", infile, inp_driz)
	        #imcopy (infile, inp_driz, verbose-)
	        printf("\n")
	        inp_driz = infile
	    }


	    # Call Drizzle.

 	    printf ("drizzle %s %s outweig=%s in_mask=%s xsh=%s ysh=%s\n\n", \
	             inp_driz, out_driz, out_wght, maskfile, str(xsh), str(ysh))
	    drizzle (inp_driz, out_driz, outweig=out_wght, in_mask=maskfile, \
	             xsh=xsh, ysh=ysh, coeffs=scoeffs)

	    # Remove the temporary input file (only if it was created in the
	    # first place!
	    if (gcount == 4) imdelete (inp_driz, verify-, >& "dev$null")

            flprcache

	}


	delete (t_list1, verify-, >& "dev$null")
	delete (t_list2, verify-, >& "dev$null")
	delete (t_list3, verify-, >& "dev$null")

	list1 = ""
	list2 = ""
	list3 = ""

	printf ("\n*** LOOP_DRIZ done ***\n")
end
