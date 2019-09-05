# cl wrapper for calstis
# S.Hulbert/R.Katsanis 17Jun97
#
procedure calstis (input)

string  input    {"", prompt = "Input STIS _raw FITS files"}
string  wavecal  {"", prompt = "Input raw wavecal image files"}
string  outroot  {"", prompt = "Root for output file names"}
bool    savetmp  {no, prompt = "Save temporary files?"}
bool    verbose  {yes, prompt = "Print verbose time stamps?"}
string  Version  {"3.4 (13-Nov-2013", prompt="calstis version"}

begin

	# local variables

	string inp
	string csstr

	# get input from par file

	inp = input

	if ( wavecal == " " )
	    wavecal = ""
	if ( outroot == " " )
	    outroot = ""

	# build command string
	# Executable should be in your path

	csstr = "!cs0.e"

	if (verbose) 
	    csstr = csstr // " -t -v"

	if (savetmp) 
	    csstr = csstr // " -s"

	csstr = csstr // " '" // inp // "'"

	if ( outroot != "" )
	    csstr = csstr // " '" // outroot // "'"

	if ( wavecal != "" )
	     csstr = csstr // " -w '" // wavecal // "'"


	if (verbose)
	    printf ("%s\n", csstr)


	# execute
	print (csstr) | cl

	# exit
	print ("")

end
