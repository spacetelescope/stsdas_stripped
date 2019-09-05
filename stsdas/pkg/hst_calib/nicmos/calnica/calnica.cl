# CL wrapper for calnica
#
# H.Bushouse     4-June-97 Initial implementation (Version 2.3)
# H.Bushouse     11-Sep-97 Added crthresh and zsthresh params (Version 3.0)
# H.Bushouse     14-Jan-98 Modified to work under VMS (Version 3.1.1)
# H.Bushouse     01-Oct-98 Added samprej task parameter (Version 3.3)
# H.Bushouse     15-Oct-98 Added barthresh task parameter (Version 3.3)
# R.Jedrzejewski 06-Feb-02 Added writedark task parameter (Version 4.0)
#
procedure calnica (input, output)
 
file    input     {"",prompt="Input file name"}
file	output    {"",prompt="Output file name"}
real	crthresh  {4.0, prompt="CR rejection threshold", min=0}
real	zsthresh  {5.0, prompt="Zero-read signal detection threshold", min=0}
real	barthresh {3.0, prompt="Bar detection threshold", min=0}
int	samprej   {0, prompt="Number of initial samples to reject", min=0}
bool    writedark {no, prompt="Write out dynamically-generated dark?"}
string  Version   {"4.4.1",prompt="Version number"}
string  Date      {"14July2009",prompt="Date of installation"}
 
begin

	# local variables
        file inp, inpx
	file out, outx
	int srej
	real crth, zsth, brth
	string doit, exec
        bool wd

	# get parameters
	inp = input
	out = output
	crth = crthresh
	zsth = zsthresh
	brth = barthresh
	srej = samprej
        wd = writedark

	# define bin directory to stsdas executables
	exec = osfn ("stsdasbin$")

	# build command string

        doit = "!" // exec // "x_calnica.e "

        inpx = inp
        outx = out
	doit = doit // inpx

	if (outx != "" && outx != " ")
	   doit = doit // " " // outx
	else
	   doit = doit // " \"\""
	doit = doit // " " // crth // " " // zsth // " " // brth // " " // srej

        if (wd)
           doit = doit // " -write"

	# execute
	print (doit) | cl
end

