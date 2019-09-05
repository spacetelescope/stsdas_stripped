# IS_REFFILE -- Return true if string is a valid reference file name

bool procedure is_reffile (reffile) 

char	reffile[ARB]	# i: string to be checked
#--
bool	found
char	dot, uscore
int	ic, jc, kc

data	dot	/ '.' /
data	uscore	/ '_' /
string	icodes  "xyzvnomuw"

int	stridx(), strldx(), strncmp()

begin
	ic = strldx (dot, reffile)
	found = false

	if (ic > 0) {
	    kc = ic + 1

	    if (strncmp (reffile[kc], "fit", 3) == 0)
		ic = strldx (uscore, reffile)

	    if (strncmp (reffile[ic+1], "wav", 3) == 0)
		found = false

	    if (ic < kc) {
		jc = stridx (reffile[ic-1], icodes)
		found = jc > 0
	    }
	}

	return (found)
end
