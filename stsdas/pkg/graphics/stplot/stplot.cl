#{ stplot package -- STSDAS vector graphics utilities.

package	stplot

task	depind,
	newcont,
	fieldplot,
	grlist,
	grplot,
	histogram,
	igi,
	rc,
        rdsiaf,
	sgraph,
	siaper,
	skymap		= "stplot$x_stplot.e"

task	grspec		= "stplot$grspec.cl"
task	stfov		= "stplot$stfov.cl"
task    psikern         = "stplot$x_psikern.e"

# Plot attributes parameters pset
task	pltpar		= "stplot$pltpar.par"

# Axis attributes parameters pset
task	axispar		= "stplot$axispar.par"

# Device parameters pset (device name, append, and viewport)
task	dvpar		= "stplot$dvpar.par"

# Psets for skymap
task	colnames	= "stplot$colnames.par"
task	catlim		= "stplot$catlim.par"
task	gsc		= "stplot$gsc.par"
hidetask	gsc

# Pset for default WCS mapping for siaper.
task	siaper_defwcs	= "stplot$siaper_defwcs.par"
hidetask siaper_defwcs

clbye()
