procedure x2d (input, output)

string input    {"", prompt="input FITS files"}
string output   {"", prompt="output FITS files"}
string helcorr  {"perform", prompt="Convert to heliocentric wavelengths?",
			min="perform|omit"}
string fluxcorr {"perform", prompt="Convert to absolute flux units?",
			min="perform|omit"}
bool statflag	{yes, prompt="Compute statistics?"}
bool center     {no,  prompt="Center target in output image?"}
real blazeshift {INDEF, prompt="blaze shift (in pixels)"}
string err_alg  {"default", prompt="how to interpolate error estimates",
			min="default|wgt_err"}
bool verbose    {yes, prompt="Print additional info?"}
string Version  {"3.4 (13-Nov-2013)", prompt="calstis version"}

begin

	string	 inp, out
	string	 x2dstr


	# Get input parameters from par file.

	inp = input
	out = output

	# build command string
	# cs7.e should be on $PATH

	x2dstr = "!cs7.e"

	x2dstr = x2dstr // " '" // inp // "' '"// out // "'"

	if (verbose)
	    x2dstr = x2dstr // " -t -v"

	if (center)
	    x2dstr = x2dstr // " -c"

	if (helcorr == "perform")
	    x2dstr = x2dstr // " -hel"

	if (fluxcorr == "perform")
	    x2dstr = x2dstr // " -flux"

	if ( statflag )
	    x2dstr = x2dstr // " -stat"

	# Currently not performed. There is no ref. file to perform this step.
	#if (sgeo == "perform")
	#    x2dstr = x2dstr // " -sgeo"

	if ( (helcorr == "omit") && (fluxcorr == "omit") && (!statflag) )
	     x2dstr = x2dstr // " -x2d"

	if (blazeshift != INDEF)
	    x2dstr = x2dstr // " -b " // blazeshift

	# The default is to weight the variance; the alternative is
	# to weight the error.
	if (err_alg != "default")
	     x2dstr = x2dstr // " -wgt_err"

	if (verbose)
	    printf ("%s\n", x2dstr)

	# execute
	print (x2dstr) | cl

	# exit
	print ("")

end
