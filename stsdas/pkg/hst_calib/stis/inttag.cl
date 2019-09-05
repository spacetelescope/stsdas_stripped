# cl wrapper for inttag
# R. Katsanis 4Jun97:
#	Initial version.
# P. Hodge 13 Oct 1997:
#	Rename sttime to starttime; use INDEF as the default for increment.
# P. Hodge 26 Jan 2000:
#	Don't use local variables for hidden parameters.
# P. Hodge 8 Sept 2000:
#	Update the Version string, since inttag.c has been modified.
# P. Hodge 13 Oct 2000:
#	Update the Version string.
# P. Hodge 26 Apr 2002:
#	Update the Version string.

procedure inttag (input, output)

file    input    	{"", prompt="File containing TIMETAG event stream"}
file	output	 	{"", prompt="Name of output file"}
real	starttime 	{INDEF, prompt="Starting time (in seconds)"}
real	increment	{INDEF, prompt="Time increment (in seconds)"}
int	rcount		{1, prompt="Number of time intervals", min=1}
bool	highres		{no, prompt="highres output?"}
bool	allevents	{no, prompt="include all events in input?"}
bool    verbose  	{yes, prompt="Print verbose messages?"}
string  Version  	{"26Apr2002", prompt="Date of Installation"}

begin

	# local variables
	string  inp, out
	string  inttagstr, exec, inpx, outx
	

	# get input & output from par file
	inp = input
	out = output

	if (rcount > 1 && increment == INDEF)
	    error (1, "increment must be specified if rcount > 1")

	if (allevents &&
		(starttime != INDEF || increment != INDEF || rcount > 1)) {
	    error (1,
	"If allevents=yes, can't specify starttime, increment, or rcount.")
	}


	# specify directory containing the executable
	exec = osfn ("stsdasbin$")
	inpx = osfn (inp)
	outx = osfn (out)


	# check if file exists
	if (!access (inpx))
	    error (1, "Input file not found.")


	# build command string
	inttagstr = "!" // exec //  "inttag.e "

	inttagstr = inttagstr // inpx // " " // outx

	if (increment != INDEF || rcount > 1) {

	    if (starttime == INDEF)
		inttagstr = inttagstr // " first"
	    else
		inttagstr = inttagstr // " " // starttime

	    inttagstr = inttagstr // " " // increment

	    if (rcount > 1)
		inttagstr = inttagstr // " " // rcount

	} else if (starttime != INDEF) {

	    inttagstr = inttagstr // " " // starttime

	}

	if (highres)
	    inttagstr = inttagstr // " -h"

	if (allevents)
	    inttagstr = inttagstr // " -a"

	if (verbose)
	    inttagstr = inttagstr // " -v"

	if (verbose)
	    printf ("%s\n", inttagstr)

	# execute
	print (inttagstr) | cl

	# exit
	print ("")

end
