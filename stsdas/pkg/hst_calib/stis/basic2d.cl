# Iraf script to run calstis1
# Created by R. Katsanis 22May97

procedure basic2d (input, output)

string input      {"", prompt="input FITS files"}
string output     {"", prompt="output FITS files"}
string outblev    {"", prompt="output text files for bias levels"}
string dqicorr    {"perform", prompt="Update dq array from bad pixel table",
			min="perform|omit"}
string atodcorr   {"omit", prompt="Analog-to-digital correction (CCD)",
			min="perform|omit"}
string blevcorr   {"perform",
			prompt="Subtract bias from overscan regions (CCD)",
			min="perform|omit"}
string doppcorr   {"perform",
		prompt="Convolve reference files w/Doppler smoothing function?",
			min="perform|omit"}
string lorscorr   {"perform", prompt="Convert from high-res to low-res (MAMA)",
			min="perform|omit"}
string glincorr   {"perform", prompt="Check for nonlinearity (MAMA)",
			min="perform|omit"}
string lflgcorr   {"perform", prompt="Check for nonlinearity (MAMA)",
			min="perform|omit"}
string biascorr   {"perform", prompt="Subtract bias image (CCD)",
			min="perform|omit"}
string darkcorr   {"perform", prompt="Subtract dark image",
			min="perform|omit"}
string flatcorr   {"perform", prompt="Divide by flat field image",
			min="perform|omit"}
string shadcorr   {"omit", prompt="Correct for shutter shading (CCD)",
			min="perform|omit"}
string photcorr   {"perform", prompt="Update photometry keywords",
			min="perform|omit"}
string darkscale  {"", prompt="list of scaling factors for the dark processing"}
bool   statflag   {yes, prompt="Compute statistics?"}
bool verbose      {yes, prompt="Print additional info?"}
string  Version   {"3.4 (13-Nov-2013)", prompt="calstis version"}

begin
	string 	inp, out
	string	basic2dstr

	# Get input parameters from par file.

	inp = input
	out = output

	# build command string
	# cs1.e should be in $PATH

	basic2dstr = "!cs1.e"

	basic2dstr = basic2dstr // " '" // inp // "' '" // out // "'"

	if (outblev != "" && outblev != " ")
	    basic2dstr = basic2dstr // " '" // outblev // "'"

	if (verbose)
	    basic2dstr = basic2dstr // " -t -v"

	if (dqicorr == "perform")
	    basic2dstr = basic2dstr // " -dqi"

	if (atodcorr == "perform")
	    basic2dstr = basic2dstr // " -atod"

	if (blevcorr == "perform")
	    basic2dstr = basic2dstr // " -blev"

	if (doppcorr == "perform")
	    basic2dstr = basic2dstr // " -dopp"

	if (lorscorr == "perform")
	    basic2dstr = basic2dstr // " -lors"

	if (glincorr == "perform")
	    basic2dstr = basic2dstr // " -glin"

	if (lflgcorr == "perform")
	    basic2dstr = basic2dstr // " -lflg"

	if (biascorr == "perform")
	    basic2dstr = basic2dstr // " -bias"

	if (darkcorr == "perform")
	    basic2dstr = basic2dstr // " -dark"

	if (flatcorr == "perform")
	    basic2dstr = basic2dstr // " -flat"

	if (shadcorr == "perform")
	    basic2dstr = basic2dstr // " -shad"

	if (photcorr == "perform")
	    basic2dstr = basic2dstr // " -phot"

	if (darkscale != "")
	    basic2dstr = basic2dstr // " -dscl '" // darkscale // "'"

	if ( statflag )
	    basic2dstr = basic2dstr // " -stat"

	if (verbose)
	    printf ("%s\n", basic2dstr)

	# execute
	print (basic2dstr) | cl

	# exit
	print ("")

end
