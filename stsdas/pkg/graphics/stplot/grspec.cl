procedure grspec (image, members)

file image {prompt="Image file name"}
string	members {prompt="Group members to plot"}

bool	inclusive = no {prompt="Include non-group files?"}

bool	autolayout = yes {prompt="Automatic layout?"}
bool	autoscale = yes	{prompt="Scale the spectra to a common mean?"}
real	fraction = 1.2 {prompt="Separation step"}
real	scale = 1 {prompt="Scaling"}
real	offset = 0 {prompt="Offset"}
real	step = 0 {prompt="Separation when not using layout"}
string	ptype = "1" {prompt="Default plotting type"}
string	labels = "user" {prompt="Spectrum labels"}
string	ulabels = "" {prompt="File containing user labels"}
real	xlpos = 1.02 {prompt="Starting position for spectrum label"}
real	ylpos = 0 {prompt="Vertical position from m.v. of spectrum"}
string	title = "" {prompt="Title"}
string	xlabel = "" {prompt="X axis label"}
string	ylabel = "" {prompt="Y axis label"}
real	xmin = INDEF {prompt="Minimum X value of initial graph"}
real	xmax = INDEF {prompt="Maximum X value of initial graph"}
real	ymin = INDEF {prompt="Minimum Y value of initial graph"}
real	ymax = INDEF {prompt="Maximum Y value of initial graph"}
string	graphics = "stdgraph" {prompt="Output graphics device"}

begin
	file    grlist
	string	pimage, pmembers, ptitle

	pimage = image
	pmembers = members

	# Check for the appropriate packages loaded.
	grlist = ""
	if (!defpac ("stplot"))
	    grlist = grlist//"stplot "
	if (!defpac ("onedspec"))
	    grlist = grlist//"onedspec "
	if (grlist != "")
	    error (1, "grspec: Please load packages: "//grlist)
	
	# Create the group listing.
	grlist  = mktemp ("grl")
	grlist (pimage, pmembers, inclusive=inclusive, >grlist)

	# If the title is empty, make it the input and group list.
	ptitle = title
	if (strlen (ptitle) <= 0)
	    ptitle = pimage+"["+pmembers+"]"
	
	specplot ("@"+grlist, autolayout=autolayout,
		  autoscale=autoscale, fraction=fraction, scale=scale,
		  offset=offset, step=step, ptype=ptype, labels=labels,
		  ulabels=ulabels, xlpos=xlpos, ylpos=ylpos, title=ptitle,
		  xlabel=xlabel, ylabel=ylabel, xmin=xmin, xmax=xmax, ymin=ymin,
		  ymax=ymax, graphics=graphics)
	
	delete (grlist)
end
