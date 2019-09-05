procedure regions (ra, dec, width, exttab)

#  REGIONS -- Extract guide stars from the CD-ROM catalog in a field.

#  Modified 6 December 1990 Z.G. Levay, STScI
#  to access NFS mounted GSC CD-ROM and simplify task structure.  
#  6 December 1991 added test for loaded packages and removed stplot
#  package load.  ZGL
#  6 May 1992 added allcols parameter for compatibility with extgst.  ZGL
## 16 June 1992 added savetabs parameter to avoid deleting and
## reextracting tables.  ZGL
## 19 Nov 92 Include all skymap parameters.  ZGL
## 1/28/93  Use utilities.lcase to force GSC path name to lower case.
## 5/8/97  Modify to use copyftt intead of fttostt RLW

real	ra	{min=0, max=24, prompt="R.A. of field center in hours"}
real	dec	{min=-90, max=90, prompt="Dec. of field center in degrees"}
real	width	{min=0,max=180, prompt="Width of field in degrees"}
file	exttab	{"extgsc", prompt="Table of extracted stars"}

real	mag1	{INDEF, prompt="Magnitude limit"}
real	mag2	{INDEF, prompt="Magnitude limit"}
bool	allcols	{no, prompt="Copy all table columns from GSC?"}
bool	savetabs	{no, prompt="Keep GSC regions tables?"}
bool	plot	{no, prompt="Plot sky chart of selected region?"}

string	version	{"19Nov92", prompt="Installed"}

struct	*tlist	{mode="h"}

string	mode	{"a"}

begin
	file	tabname		# Table root name
	file	fitstab		# FITS table name
	file	sdastab		# STSDAS table name
	file	sdashdr

	file	ftlist
	file 	path
	file	st_tables
	file	gsctab

	string	extension

	real	racen
	real	decen
	real	size

	bool	first
	bool	cpallc


	if (plot && !defpac("stplot"))
	    error (1, "stplot package needs to be loaded!")

	racen	= regions.ra
	decen	= regions.dec
	size	= regions.width
	gsctab	= regions.exttab
	cpallc  = regions.allcols

	ftlist  = mktemp ("ftl")
	st_tables = mktemp ("tmp$tab")

	print ("Searching index file for GSC regions")
	sgscind (racen, decen, size, tablist=ftlist)

	if (sgscind.nregions == 0) {
	    delete (ftlist)
	    error (1, "No GSC regions found")
	}

	# Copying FITS region table(s) to local area files with right extensions
	lcase (ftlist) | copyftt (st_tables)

	# Use cl parameters to get around bug with INDEF
	x = regions.mag1
	y = regions.mag2

	if (x == y) {
	    x = INDEF
	    y = INDEF
	}

	print ("Extracting stars from the region table(s)")
	extgst ("@"+st_tables, gsctab,
	    racen, decen, size, mag1=x, mag2=y, allcols=cpallc)

	if (extgst.nstars == 0) {
	    delete (ftlist)
	    delete (st_tables)
	    error (2, "No guide stars selected")
	}

	tlist = st_tables
	while (fscan (tlist, tabname) != EOF) {
	    # For each table in the list

	    if (!savetabs) {
		# delete the local FITS table
		sdastab = tabname
		delete (sdastab)
	    }

	    # Get rid of extraneous headers
	    extension = envget ("imtype")
	    sdashdr = tabname + "." + extension
	    if (access (sdashdr)) {
		unprotect (sdashdr)
		delete (sdashdr)
	    }
	}

	if (plot) {
	    # Draw a chart of the region
	    skymap (gsctab, racen, decen, INDEF, size, colnames="gsc",
	       catlim="", title="", stars=yes, grid=yes, key=yes,
	       objstyle="filled", color=1, bigspot=0.03,
	       smallspot=2.0E-4, brightstar=INDEF, faintstar=INDEF,
	       label=no, format="s=.75", device="stdgraph", append=no,
	       interactive=no, coords="")
	}

	delete (ftlist)
	delete (st_tables)
end
