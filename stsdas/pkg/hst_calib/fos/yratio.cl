#---------------------------------------------------------------------------
# .help yratio 20Nov95 fos
# .ih
# NAME
# yratio - Calculate normalized accums.
# .endhelp
#---------------------------------------------------------------------------
procedure yratio (input, output)

file	input	{prompt="List of observations to ratio the individual groups"}
file	output	{prompt="List of output observations that have the ratio taken"}

bool	verbose	{yes,prompt="Show progress of script"}

struct	*in_list {mode="h"}
struct	*out_list {mode="h"}

begin
	# Declarations
	string	exp
	int	gcount
	string	gspec
	int	i
	file	in_file
	file	in_file_1
	file	in_list_file
	bool	noexp
	bool	nonorm
	file	norm
	file	norm_gspec
	file	out_file
	file	out_list_file
	file	pinput
	file	poutput
	file	tmproot
	
	# Get interactive parameters.
	pinput = input
	poutput = output

	# Make file names.
	tmproot = mktemp ("tmp$YRATIO")
	in_list_file = tmproot//"_ilf.txt"
	norm = tmproot//"_n.hhh"
	out_list_file = tmproot//"_olf.txt"
	
	# Make a list of the input/output specta
	files (pinput, sort=yes, > in_list_file)
	files (poutput, sort=yes, > out_list_file)

	# For each input.  Divide each group by the first group, the first
	# group being normalized to the exposure time for each subsequent
	# group.
	in_list = in_list_file
	out_list = out_list_file
	while (fscan (in_list, in_file) != EOF) {

	    # Get output file.
	    if (fscan (out_list, out_file) == EOF)
		error (1, "no more output files specified")

	    if (verbose)
		print ("yratio: "//in_file//" --> "//out_file)

	    # Check for FOS data.  Too many assumptions here for other types.
	    keypar (in_file, "instrume", silent=yes)
	    if (!keypar.found || keypar.value != "FOS") {
		print ("WARNING: Image is not FOS and therefore cannot be input to this task.")
		print ("    Skipping to next input image...")
		next
	    }

	    # Determine normalization factor.  If count->count rate conversion
	    # has been done, there is no need to normalize.
	    keypar (in_file, "cnt_corr", silent=yes)
	    if (!keypar.found) {
		print ("WARNING: 'CNT_CORR' keyword not present implying that the input is non-FOS data.")
		print ("    Cannot run yratio, skipping to next input image...")
		next
	    }
	    nonorm = (keypar.value == "COMPLETE")
	    keypar (in_file, "exposure", silent=yes)
	    if (!keypar.found) {
		print ("WARNING: 'EXPSOSURE' keyword not presetn implying that the input is non-FOS data.")
		print ("    Cannot run yratio, skipping to next input image...")
		next
	    }
	    exp = keypar.value
	    noexp = (real (exp) == 0.)
	    
	    # Setup the normalized image.
	    imdelete (norm//"*", yes, verify=no, default_action=yes,
		      >& "dev$null")
	    keypar (in_file, "gcount", silent=yes)
	    if (!keypar.found) {
		print ("WARNING: Keyword GCOUNT not found in image '"//in_file)
		print ("    in_file is not mulitgroup; nothing to ratio.  Skipping to next file...")
		next
	    }
	    gcount = int (keypar.value)
	    imcopy (in_file, norm//"[1/"//gcount//"]", verbose=no)
	    in_file_1 = in_file//"[1]"
	    if (verbose) {
		if (nonorm)
		    print ("yratio: Input has already been normalized, no exposure correction required.")
		else if (noexp)
		    print ("yratio: Input has no exposure information, assuming standard ACCUM.")
		else
		    print ("yratio: Input has exposure information, normalizing by exposure time.")
	    }
	    for (i=1; i <= gcount; i = i + 1) {
		gspec =  "["//i//"]"
		norm_gspec= norm//gspec
		if (nonorm) {
		    imcopy (in_file_1, norm_gspec, verbose=no)
		} else if (noexp) {
		    imcalc (in_file_1, norm_gspec, "im1*"//i, pixtype="old",
			    nullval=0., verbose=no)
		} else {
		    keypar (in_file//gspec, "exposure", silent=yes)
		    imcalc (in_file_1, norm_gspec,
			    "im1*"//keypar.value//"/"//exp,
			    pixtype="old", nullval=0., verbose=no)
		}
	    }

	    # Now ratio all the groups.
	    imcalc (in_file//","//norm, out_file, "im1/im2", pixtype="old",
		    nullval=0., verbose=no)
	}

	# That's all folks.
	delete (tmproot//"*", go_ahead=yes, verify=no)
end
#---------------------------------------------------------------------------
# End of yratio
#---------------------------------------------------------------------------
