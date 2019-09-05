procedure yddintplot (input)

file	input	{prompt="Observation to overlay where the dead diodes are"}
bool	useddt	{yes,prompt="Plot diodes from a dead diode image?"}
file	ddt	{"",prompt="Dead diode image to use"}
string	device	{"stdgraph",prompt="Output device"}
gcur	*cursor	{"",prompt="Graphics cursor input"}

int	diode	{INDEF,prompt="A specific diode to plot",mode="q"}
int	ltype	{0,prompt="IGI linetype to draw specific diode in",mode="q"}
int	color	{1,prompt="IGI color to draw specific diode in",mode="q"}

bool	debug	{no,prompt="Echo debugging statements"}

struct	*ddt_list

begin
	# Declarations.
	int	curp
	char	curs
	real	curx, cury
	file	dd_only, dd_onlyt1, dd_onlyt2
	int	ddt_diode
	int	done
	int	flag
	int	hdis
	int	i
	file	igi_ddplot
	file	igi_script
	int	mid
	int	npix
	int	pcolor
	int	pdiode
	file	pinput
	int	pltype
	bool	puseddt
	bool	redraw
	int	sbeg, send
	string	sx
	file	tmproot
	file	tinput
	file	ttable
	int	win
	real	ypos

	# Check that the packages are loaded.
	tinput = ""
	if (!defpac ("tools"))
	    tinput = tinput//"tools "
	if (!defpac ("imgtools"))
	    tinput = tinput//"imgtools "
	if (!defpac ("tables"))
	    tinput = tinput//"tables "
	if (!defpac ("fos"))
	    tinput = tinput//"fos "
	if (!defpac ("stplot"))
	    tinput = tinput//"stplot "
	if (tinput != "")
	    error (1, "yddintplot: load the following packages: "//tinput)
	
	# Get interactive parameters.
	pinput = input
	pdiode = INDEF
	puseddt = useddt
	pcolor = 1
	pltype = 0
	
	# Make files.
	tmproot = mktemp ("tmp$YDDINTPLOT")
	dd_only = tmproot//"_donly.txt"
	dd_onlyt1 = tmproot//"_ddonlyt1.txt"
	dd_onlyt2 = tmproot//"_ddonlyt2.txt"
	igi_ddplot = tmproot//"_ddplot.igi"
	igi_script = tmproot//".igi"
	ttable = tmproot//"_ttable.tab"
	
	# Get observation.
	# First attempt as specified.  If not found, try some default
	# extensions
	if (debug)
	    print ("yddintplot: Getting observation")
	fparse (pinput, verbose=no)
	tinput = fparse.directory//fparse.root//fparse.ext
	if (!access (tinput)) {
	    if (fparse.ext == "") {
		tinput = fparse.directory//fparse.root//".c1h"
		if (!access(tinput)) {
		    tinput = fparse.directory//fparse.root//".d0h"
		    if (!access(tinput))
			error (1, "yd2p: cannot access image "//pinput)
		}
	    } else
		error (1, "yd2p: cannot access image "//pinput)
	}
	if (fparse.cl_index > 0)
	    tinput = tinput//"["//fparse.cl_index//"]"

	# Create a table of the image and get initial section.
	imtab (tinput, ttable, "y", pname="x", wcs="logical", formats="",
	       tbltype="default")
	sbeg = 1
	keypar (tinput, "naxis1", silent=yes)
	send = int (keypar.value)
	npix = send

	# Create a table of the dead diodes and an igi macro to draw
	# them
	if (debug)
	    print ("yddintplot: Creating dd tables")
	print ("define ddplot", >> igi_ddplot)
	if (puseddt) {
	    sx = ddt
	    if (sx == "") {
		keypar (tinput, "ddthfile")
		if (keypar.found)
		    sx = keypar.value
		else {
		    printf ("yddintplot: WARNING: cannot access DDTHFILE keyword in image %s\n\tDiodes from the dead diode table cannot be plotted\n", tinput)
		    puseddt = no
		}
	    }
	    if (puseddt) {

		if (debug)
		    print ("yddintplot: Creating dd igi macro")

		# Create the text table.
		imtab (sx, dd_onlyt1, "c1", pname="c2", wcs="logical",
		       formats="", tbltype="text")
		tselect (dd_onlyt1, dd_onlyt2, "c1==0")
		tdump (dd_onlyt2, cdfile="", pfile="", datafile=dd_only,
		       columns="c2", rows="-", pwidth=80)
		
		# Create the macro.
		if (debug)
		    print ("yddintplot: looping through the igi macro")
		puseddt = no
		ddt_list = dd_only
		while (fscan (ddt_list, ddt_diode) != EOF) {
		    yd2p (ddt_diode, obs=tinput, verbose=no)
		    if (yd2p.pbeg != INDEF && yd2p.pend != INDEF) {
			puseddt = yes
			print ("move "//yd2p.pbeg//" &1", >> igi_ddplot)
			print ("draw "//yd2p.pend//" &1", >> igi_ddplot)
		    }
		}
	    }
	}

	# Confirm that a dead diode table was found.  If not, default
	# the rest.
	if (!puseddt) {
	    print ("move &1 &1", >> igi_ddplot)
	}

	# Finish off the igi macro.
	print ("end", >> igi_ddplot)

	# Graphics loop.
	win = INDEF
	redraw = yes
	if (debug)
	    print ("yddintplot: start graphics loop")
	goto refresh
	while (fscan (cursor, curx, cury, i, curs) != EOF ) {
	    curs = substr (curs, 1, 1)
	    curp = max (1, min (npix, int (curx + 0.5)))
	    redraw = yes
	    if (win != INDEF && curs != "w")
		win = INDEF
	    switch (curs) {
	    case 44: # ','
		{
		    mid = send - sbeg
		    hdis = mid * 0.9
		    sbeg = max (1, sbeg - hdis)
		    send = min (npix, sbeg + mid)
		}
		
	    case 46: # '.'
		{
		    mid = send - sbeg
		    hdis = mid * 0.9
		    send = min (npix, send + hdis)
		    sbeg = max (1, send - mid)
		}

	    case 63: # '?'
		{
		    page ("fos$doc/yddintplot.key", map_cc=yes,
			  clear_screen=yes, first_page=1, prompt="",
			  device="terminal")
		    redraw = no
		}
		
	    case 99: # 'c'
		pcolor = color
		
	    case 100: # 'd'
		{
		    sx = ""
		    if (puseddt) {
			yp2d (curp, obs=tinput, verbose=no)
			ddt_list = dd_only
			while (fscan (ddt_list, ddt_diode) != EOF) {
			    if (ddt_diode >= yp2d.dbeg &&
				ddt_diode <= yp2d.dend)
				sx = sx//" "//ddt_diode
			}
		    }
		    if (sx != "")
			print ("yddintplot: pixel "//curp//" is affected by dead diodes: "//sx)
		    else
			print ("yddintplot: pixel "//curp//" is not affected by known dead diodes.")
		    redraw = no
		}
		
	    case 102: # 'f'
		{
		    sbeg = 1
		    send = npix
		}

	    case 108: # 'l'
		pltype = ltype

	    case 111: # 'o'
		{
		    mid = (sbeg + send) / 2
		    hdis = send - sbeg
		    send = min (npix, mid + hdis)
		    sbeg = max (1, mid - hdis)
		}

	    case 112: # 'p'
		{
		    yp2d (curp, obs=tinput, verbose=no)
		    printf ("yddintplot: Diodes %d-%d contribute to pixel %d\n",
			   yp2d.dbeg, yp2d.dend, curp)
		    redraw = no
		}
		
	    case 113:	# 'q'
		break

	    case 114: # 'r'
		# This simply refreshes the plot.
		;

	    case 115: # 's'
		pdiode = diode
		
	    case 119: # 'w'
		if (win == INDEF) {
		    win = curp
		    redraw = no
		    print ("yddintplot: Reposition cursor and hit 'w' again")
		} else {
		    sbeg = min (win, curp)
		    send = max (win, curp)
		    win = INDEF
		}

	    case 122: # 'z'
		{
		    mid = (sbeg + send) / 2
		    hdis = max (1, (send - mid) * 0.5)
		    send = mid + hdis
		    sbeg = mid - hdis
		}

	    default:
		{
		    print ("yddintplot: Type '?' for a list of key commands")
		    redraw = no
		}
	    }

	    # Determine the Y position of the diode markers.
refresh:
	    if (redraw) {
		tstat (ttable, "y", outtable="", lowlim=INDEF,
		       highlim=INDEF, rows=sbeg//"-"//send)
		ypos = ((tstat.vmax-tstat.vmin)*0.05)+tstat.vmin

		# Build the igi script to redraw the screen.
		delete (igi_script, verify=no, >& "dev$null")
		copy (igi_ddplot, igi_script, verbose=no)

		# Draw the data.
		print ("data "//ttable, >> igi_script)
		print ("lines "//sbeg//" "//send, >> igi_script)
		print ("xcolumn x1; ycolumn y", >> igi_script)
		print ("limits; box; connect", >> igi_script)

		# Draw the dead diode tables.
		print ("ddplot "//ypos, >> igi_script)

		# If a specific diode is present, draw it.
		if (pdiode != INDEF) {
		    print ("ltype "//pltype, >> igi_script)
		    print ("color "//pcolor, >> igi_script)
		    yd2p (pdiode, obs=tinput, verbose=no)
		    print ("move "//yd2p.pbeg//" "//ypos, >> igi_script)
		    print ("draw "//yd2p.pend//" "//ypos, >> igi_script)
		}

		# Execute the script.
		igi (initcmd="", wlpars="", usewcs=no, wcspars="",
		     device=device, metacode="", append=no, debug=no,
		     cursor="", < igi_script)
	    }
	}
	
	# That's all folks.
	ddt_list = ""
	delete (tmproot//"*", verify=no, >& "dev$null")
end
