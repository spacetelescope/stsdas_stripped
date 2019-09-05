procedure yd2p (diode)
int	diode	{min=1,max=512,prompt="Input diode number to find which pixels are affected"}
file	obs	{"",prompt="Observation to get step pattern from"}
file	dlist	{"",prompt="List of diodes to convert"}
int	ystep	{1,prompt="Which ystep should be referred to"}
int	nxsteps	{4,prompt="NX steps if no obs specified"}
int	overscan {5,prompt="OVERSCAN if no obs specified"}
int	fchnl	{1,prompt="First channel (diode) to be read"}
int	nchnls	{512, prompt="Number of channels (diodes) to be read"}
int	pbeg	{prompt="Output: First pixel specified diode contributes to"}
int	pend	{prompt="Output: Last pixel specified diode contributes to"}
bool	verbose	{yes,prompt="Write results to standard output"}

struct	*dlst

begin
	# Declarations
	struct	inline
	int	pdiode
	int	pfchnl, plchnl, pnchnls
	int	pnx
	file	pobs
	int	pos
	file	tobs
	int	yoff

	# Get interactive parameters
	if (dlist == "")
	    pdiode = diode
	pobs = obs
	if (pobs == "") {
	    pnx = nxsteps
	    pos = overscan
	    pfchnl = fchnl
	    pnchnls = nchnls
	}

	# If an observation is specified, try to get pattern information.
	if (pobs != "") {

	    # First attempt as specified.  If not found, try some default
	    # extensions
	    tobs = pobs
	    keypar (tobs, "nxsteps", >& "dev$null")
	    if (!keypar.found) {
		fparse (pobs, verbose=no)
		if (fparse.ext == "") {
		    tobs = fparse.directory//fparse.root//".c1h"
		    keypar (tobs, "nxsteps", >& "dev$null")
		    if (!keypar.found) {
			tobs = fparse.directory//fparse.root//".d0h"
			keypar (tobs, "nxsteps", >& "dev$null")
			if (!keypar.found)
			    error (1, "yd2p: cannot access image "//pobs)
		    }
		}
	    }
	    pnx = int (keypar.value)
	    keypar (tobs, "overscan", >& "dev$null")
	    if (!keypar.found)
		error (1, "yd2p: cannot access OVERSCAN parameter in image "//tobs)
	    pos = int (keypar.value)
	    keypar (tobs, "fchnl", >& "dev$null")
	    if (!keypar.found)
		error (1, "yd2p: cannot access FCHNL parameter in image "//tobs)
	    pfchnl = int (keypar.value)
	    keypar (tobs, "nchnls", >& "dev$null")
	    if (!keypar.found)
		error (1, "yd2p: cannot access NCHNLS parameter in image "//tobs)
	    pnchnls = int (keypar.value)
	}

	# Figure out diode range.
	plchnl = pfchnl+pnchnls-1

	# If just a single pixel, convert.
	yoff = (ystep-1) * (((pnchnls-1) * pnx) + (pnx * pos))
	if (verbose) {
	    print ("# yd2p: nxsteps = "//pnx//" overscan = "//pos//" ystep = "//ystep//" fchnl = "//pfchnl//" nchnls = "//pnchnls)
	    print ("# diode  pbeg  pend")
	}
	if (dlist == "") {
	    if (pdiode < pfchnl || pdiode > plchnl) {
		pbeg = INDEF
		pend = INDEF
		if (verbose)
		    printf ("#  %3.3d   INDEF INDEF\n", pdiode)
	    } else {
		pbeg = (pdiode-pfchnl) * pnx + 1 + yoff
		pend = pbeg + (pnx * pos) - 1
		if (verbose)
		    printf ("   %3.3d   %4.4d  %4.4d\n", pdiode, pbeg, pend)
	    }
	}

	# Else, read the list.  Any comment lines are ignored.
	else {
	    dlst = dlist
	    while (fscan (dlst, inline) != EOF) {
		if (substr (inline,1,1) == "#")
		    next
		if (fscan (inline, pdiode) < 1)
		    next
		if (pdiode < pfchnl || pdiode > plchnl) {
		    pbeg = INDEF
		    pend = INDEF
		    if (verbose)
			printf ("#  %3.3d   INDEF INDEF\n", pdiode)
		} else {
		    pbeg = (pdiode-pfchnl) * pnx + 1
		    pend = pbeg + (pnx * pos) - 1
		    if (verbose)
			printf ("   %3.3d   %4.4d  %4.4d\n", pdiode, pbeg, pend)
		}
	    }
	    dlst = ""
	}
end
