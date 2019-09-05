procedure _cs12 (input, wavecal)

string input	{"", prompt="Input calibrated science files"}
string wavecal  {"", prompt="Input calibrated & 2-D rectified wavecal files"}
bool   verbose  {yes, prompt="Print additional info?"}
string option	{"linear", prompt="Interpolation option",
			min = "nearest|linear"}
string Version  {"3.4 (13-Nov-2013)", prompt="calstis version"}

begin

	string	 wave, inp
	string	 cs12str


	# Get input parameters from par file.

	inp = input
	wave = wavecal


	# build command string
	# cs12.e should be on $PATH

	cs12str = "!cs12.e"

	if (verbose)
	    cs12str = cs12str // " -t -v "

	cs12str = cs12str // " " // wave // " " // inp

	cs12str = cs12str // " " // option

	if (verbose)
	    printf ("%s\n", cs12str)

	# execute
	print (cs12str) | cl

	# exit
	print ("")

end
