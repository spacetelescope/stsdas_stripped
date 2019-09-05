procedure x1d (input)

string	input    {"", prompt="Name of input FITS files"}
string	output   {"", prompt="Name of output FITS files to contain 1-D spectra"}
string	backcorr {"perform", prompt="Subtract background (sky and interorder)?",
                              min="perform|omit"}
string  ctecorr   {"perform", prompt="Apply CTE correction?",
                              min="perform|omit"}
string	dispcorr  {"perform", prompt="Apply 2-D dispersion solutions?",
                              min="perform|omit"}
string	helcorr   {"perform", prompt="Convert to heliocentric wavelengths?",
                              min="perform|omit"}
string	fluxcorr  {"perform", prompt="Convert to absolute flux units?",
                              min="perform|omit"}
int	sporder	  {INDEF, prompt="spectral order to extract"}
real	a2center  {INDEF, prompt="extract one spectrum at this Y location"}
int	maxsrch	  {INDEF, prompt="cross correlation range"}
bool	globalx	  {no,    prompt="use global crosscor offset in all orders ?"}
real	extrsize  {INDEF, prompt="size of extraction box"}
real	bk1size	  {INDEF, prompt="size of 1st background region"}
real	bk2size	  {INDEF, prompt="size of 2nd background region"}
real	bk1offst  {INDEF, prompt="offset of 1st background region"}
real	bk2offst  {INDEF, prompt="offset of 2nd background region"}
real	bktilt	  {INDEF, prompt="background tilt"}
int	backord   {INDEF, prompt="background order",min=0,max=1}
string	bksmode   {"median", prompt="Background smoothing mode",
                             min="off|median|average"}
int	bksorder  {3, prompt="Background smoothing polynomial order",
                      min=0}
real	blazeshift {INDEF, prompt="blaze shift (in pixels)"}
string	algorithm	{"unweighted", prompt="extraction algorithm",
                          min="unweighted|sc2d"}
real	xoffset	{INDEF, prompt="offset in X for slitless extraction"}

bool	verbose  {yes, prompt="verbose time stamps + more info"}
string  Version  {"3.4 (13-Nov-2013)", prompt="calstis version"}

begin
	string	 inp, out
	string	 x1dstr

	# Get input parameters from par .

	inp = input  ;  out = output

        # build command string
	# cs6.e should be on $PATH

        x1dstr = "!cs6.e"

	x1dstr = x1dstr // " '" // inp // "' '" // out // "'"

        if (verbose)
            x1dstr = x1dstr // " -t -v"

	if (globalx)
	    x1dstr = x1dstr // " -g "

	if (backcorr == "perform")
	    x1dstr = x1dstr // " -back"

        if (ctecorr == "perform")
            x1dstr = x1dstr // " -cte"

	if (dispcorr == "perform")
	    x1dstr = x1dstr // " -disp"

        if (helcorr == "perform")
            x1dstr = x1dstr // " -hel"

        if (fluxcorr == "perform")
            x1dstr = x1dstr // " -flux"

	if ( backcorr == "omit" && dispcorr == "omit" &&
		helcorr == "omit" && fluxcorr == "omit" &&
                ctecorr == "omit")
	     x1dstr = x1dstr // " -x1d"

	# do the switches
	if (sporder != INDEF)
	    x1dstr = x1dstr // " -s " // sporder

	if (a2center != INDEF)
	    x1dstr = x1dstr // " -c " // a2center

	if (maxsrch != INDEF)
	    x1dstr = x1dstr // " -r " // maxsrch

	if (extrsize != INDEF)
	    x1dstr = x1dstr // " -x " // extrsize

	if (bk1size != INDEF)
	    x1dstr = x1dstr // " -b1 " // bk1size

	if (bk2size != INDEF)
	    x1dstr = x1dstr // " -b2 " // bk2size

	if (bk1offst != INDEF)
	    x1dstr = x1dstr // " -o1 " // bk1offst

	if (bk2offst != INDEF)
	    x1dstr = x1dstr // " -o2 " // bk2offst

	if (bktilt != INDEF)
	    x1dstr = x1dstr // " -k " // bktilt

	if (backord != INDEF)
	    x1dstr = x1dstr // " -n " // backord

	if (blazeshift != INDEF)
	    x1dstr = x1dstr // " -bs " // blazeshift

	if (bksmode == "median")
	     x1dstr = x1dstr // " -bm -bo " // bksorder
	if (bksmode == "average")
	     x1dstr = x1dstr // " -bb -bo " // bksorder
	if (bksmode == "off")
	     x1dstr = x1dstr // " -bn "

        if (algorithm == "unweighted")
	    x1dstr = x1dstr // " -a unweighted "
        else if (algorithm == "sc2d")
            x1dstr = x1dstr // " -a unweighted -idt "

	if (xoffset != INDEF)
	    x1dstr = x1dstr // " -st " // xoffset


        # execute
	printf ("%s\n", x1dstr)        # to display the string, not run it
	print (x1dstr) | cl

	print ("")

end
