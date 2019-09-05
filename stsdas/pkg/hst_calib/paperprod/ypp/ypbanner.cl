procedure ypbanner (input, output, imtype)

char	input	{prompt="Observation rootname"}
char	output	{prompt="Name of the output igi script for the banner macro"}
char	imtype	{prompt="Image type of root (FITS or GEIS)"}

begin
	# Declarations
	char	d0h			# D0H image.
	real	linenum			# Line numbers.
	char	list			# Temporary list file.
	char	pinput			# Input parameter.
	char	poutput			# Output parameter.
	int	proposid		# Proposal ID.
	char	rootname		# Rootname from header.
	char	ftype

	# Get interactive parameters
	pinput = input
	poutput = output
	ftype = imtype
	
	if (ftype == "geis") {
		d0h = pinput//".d0h"
	} else {
		d0h = pinput//"_d0f.fits[0]"
	}

	keypar (d0h, "rootname", silent=yes)
	rootname = keypar.value
	keypar (d0h, "proposid", silent=yes)
	proposid = int (keypar.value)
	keypar (d0h, "linenum", silent=yes)
	linenum = real (keypar.value)
	list = mktemp ("tmp$PPYBAN")//"_list.txt"

	print ("reset; fontset hard; vpage 0.0 1.0 0.05 0.98; expand 1.",
	       >> poutput)
	print ("location 0 1 0 1", >> poutput)
	print ("!printf (\"0 .94\\n1 .94\\n1 1\\n0 1\\n\",> \""//list//"\")",
	       >> poutput)
	printf ("data %s\n", list, >> poutput)
	printf ("xcol c1; ycol c2; color 5; fillpat 2; polygon\n", >> poutput)
	printf ("color 1; fillpat 1\n", >> poutput)
	printf ("!delete %s verify-\n", list, >> poutput)
	printf ("vmove 0.02 .98; justify 3; label \"%sLogsheet Line# %.3f\"\n",
		"\\fB", linenum, >> poutput)
	printf ("vmove 0.3 .98; justify 3; label \"%sObservation: %s\"\n",
		"\\fB", rootname, >> poutput)
	printf ("vmove 0.85 .98; justify 1; label \"%sProposal: %d\"\n",
		"\\fB", proposid, >> poutput)
	print ("vmove .98 .98; justify 1; label \"\\fIFOS\"", >> poutput)
	print ("vpage 0.05 0.95 0.05 0.93; expand .9",
	       >> poutput)
end
