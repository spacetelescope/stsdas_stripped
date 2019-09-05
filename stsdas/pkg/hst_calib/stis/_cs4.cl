procedure _cs4 (wavecal)

string  wavecal    {"", prompt="Input calibrated & 2-D rectified wavecal file"}
string	debugfile  {"", prompt = "File for debug output"}
real    angle      {INDEF, prompt = "angle of long slit used with echelle",
			min=-90., max=90.}
bool    verbose    {yes, prompt="Print additional info?"}
string  Version    {"3.4 (13-Nov-2013)", prompt="calstis version"}

begin

	string	 wave
	string	 cs4str


	# Get input parameter from par file.

	wave = wavecal


	# build command string
	# cs4.e should be on $PATH

	cs4str = "!cs4.e"

	if (verbose)
	    cs4str = cs4str // " -t -v"

	cs4str = cs4str // " " // wave

	if (debugfile != "")
	    cs4str = cs4str // " -d " // debugfile

	if (angle != INDEF)
	    cs4str = cs4str // " -angle " // str (angle)

	if (verbose)
	    printf ("%s\n", cs4str)

	# execute
	print (cs4str) | cl

	# exit
	print ("")

end
