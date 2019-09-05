procedure _cs11 (input, wavecal, output)

string  input	    {"", prompt="Input calibrated science files"}
string  wavecal     {"", prompt="Input calibrated wavecal files"}
string  output      {"", prompt="Output wavecal files"}
bool    verbose     {yes, prompt="Print additional info?"}
string  Version     {"3.4 (13-Nov-2013)", prompt="calstis version"}

begin

	string	 wave, inp, out
	string	 cs11str


	# Get input parameters from par .

	inp = input
	wave = wavecal
	out = output


	# build command string
	# cs11.e should be on $PATH

	cs11str = "!cs11.e"

	if (verbose)
	    cs11str = cs11str // " -t -v"

	cs11str = cs11str // " " // wave // " " // inp // " " // out


	if (verbose)
	    printf  ("%s\n", cs11str)

	# execute
	print (cs11str) | cl

	# exit
	print ("")

end
