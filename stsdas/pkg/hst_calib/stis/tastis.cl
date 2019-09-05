procedure tastis (input)

string input = "" {prompt = "input raw.fits files"}
bool   update = no {prompt = "update ACQSTAT keyword in input files?"}
string Version = "5September2003" {prompt = "tastis version"}
string mode = "al"

begin
	string inp
	bool l_update
	string csstr, exec

	# get input from par file
	inp = input
	l_update = update

	# this is the location of the tastis executable
	exec = osfn ("stsdasbin$")

	# build command string
	csstr = "!" // exec // "x_tastis.e"
	if (l_update) {
	    csstr = csstr + " -u"
	}
	csstr = csstr + " '" // inp // "'"

	printf ("%s\n", csstr)

	print (csstr) | cl
end
