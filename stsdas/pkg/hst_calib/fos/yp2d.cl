procedure yp2d (pixel)
int    	pixel	{prompt="Input pixel number"}
file	obs	{"",prompt="Observation to get step pattern from"}
file	plist	{"",prompt="File containing a list of pixels to convert"}
int	nxsteps	{4,prompt="NX steps if no obs specified"}
int	overscan {5,prompt="OVERSCAN if no obs specified"}
int	fchnl	{1,prompt="First channel (diode) to be read"}
int	nchnls	{512, prompt="Number of channels (diodes) to be read"}
int	dbeg	{prompt="Output: First diode contributing to pixel"}
int	dend	{prompt="Output: Last diode contributing to pixel"}
bool	verbose	{yes,prompt="Write results to standard output"}

struct	*plst

begin
	# Declarations
	struct	inline
	int	npix
	int	p
	int	pfchnl, plchnl, pnchnls
	int	pnx
	file	pobs
	int	pos
	int	ppixel
	file	tobs

	# Get interactive parameters
	if (plist == "")
	    ppixel = pixel
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
			    error (1, "yp2d: cannot access image "//pobs)
		    }
		}
	    }
	    pnx = int (keypar.value)
	    keypar (tobs, "overscan", >& "dev$null")
	    if (!keypar.found)
		error (1, "yp2d: cannot access OVERSCAN parameter in image "//tobs)
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

	# If no list is specified, translate the one pixel.
	npix = ((pnchnls-1) * pnx) + (pnx * pos)
	if (verbose) {
	    print ("# yp2d: nxsteps = "//pnx//" overscan = "//pos//" fchnl = "//pfchnl//" nchnls = "//pnchnls)
	    print ("# pixel  dbeg  dend")
	}
	if (plist == "") {
	    p = (ppixel-1) % npix
	    dend = (p / pnx) + pfchnl
	    dbeg = max (pfchnl, dend-pos+1)
	    dend = min (dend, plchnl)
	    if (verbose)
		printf ("   %4.4d   %3.3d   %3.3d\n", ppixel, dbeg, dend)
	}

	# Else, loop through the specified list.
	else {
	    plst = plist
	    while (fscan (plst, inline) != EOF) {
		if (substr (inline,1,1) == "#")
		    next
		if (fscan (inline, ppixel) < 1)
		    next
		p = (ppixel-1) % npix
		dend = (p / pnx) + pfchnl
		dbeg = max (pfchnl, dend-pos+1)
		dend = min (dend, plchnl)
		if (verbose)
		    printf ("   %4.4d   %3.3d   %3.3d\n", ppixel, dbeg, dend)
	    }
	    plst = ""
	}
end
