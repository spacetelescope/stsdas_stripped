# IRAF cl wrapper for calnicb
#
# H.Bushouse	 04-Jun-1997	Initial implementation (Version 2.0)
# H.Bushouse	 01-Dec-1997	Added subbkg, meanbkg, crthresh, and xcwin
#		 		params (Version 2.2)
# H.Bushouse	 17-Mar-1998	Added readbkg and readoffsets params (Vsn 2.2)
# H.Bushouse	 06-May-1998	Modified to work under VMS (Version 2.2)
# R.Jedrzejewski 26-Jan-2007    Removed VMS-enabling lines
#
procedure calnicb (input)
 
file    input       {"",  prompt="Input association table name"}
bool    subbkg      {yes, prompt="Subtract scalar background?"}
bool    meanbkg     {yes, prompt="Use mean scalar background value?"}
bool	readbkg     {no,  prompt="Read bkg values from assoc table?"}
bool	readoffsets {no,  prompt="Read offsets from assoc table?"}
real    crthresh    {5.0, prompt="CR rejection threshold", min=0}
int     xcwin       {3,   prompt="Xcorr window half width (pixels)", min=1}
string  Version     {"2.7",prompt="Version number"}
string  Date        {"06Nov2008",prompt="Date of installation"}
 
begin

	# local variables
        file inp, inpx
	bool sbb, mnb, rdb, rdo
	real crt
	int xcw
	string doit, exec

	# get parameters
	inp = input
	sbb = subbkg
	mnb = meanbkg
	rdb = readbkg
	rdo = readoffsets
	crt = crthresh
	xcw = xcwin

	# define bin directory to stsdas executables
	exec = osfn ("stsdasbin$")

	# build command string
	doit = "!"//exec//"/x_calnicb.e "

	inpx = osfn (inp)

	doit = doit // inpx

	doit = doit//" "//sbb//" "//mnb//" "//rdb//" "//rdo//" "//crt//" "//xcw

	# execute
	print (doit) | cl

end

