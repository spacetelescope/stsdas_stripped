procedure stis()
string	mode="al"

begin

	package stis

	task calstis	= "stis$calstis.cl"
	task ctestis	= "stis$ctestis.cl"
	task basic2d	= "stis$basic2d.cl"
	task doppinfo	= "stis$doppinfo/x_doppinfo.e"
        task echplot    = "stis$echplot.cl"
        task echscript  = "stis$echscript/x_echscript.e"
	task infostis	= "stis$x_infostis.e"
	task ocrreject	= "stis$ocrreject.cl"
	task odelaytime	= "stis$odelaytime/x_odelaytime.e"
        task ovac2air   = "stis$ovac2air/x_ovac2air.e"
	task inttag	= "stis$inttag.cl"
	task sdqflags	= "stis$sdqflags.cl"
        task tastis     = "stis$tastis.cl"
	task wavecal	= "stis$wavecal.cl"
	task x1d	= "stis$x1d.cl"
	task x2d	= "stis$x2d.cl"
	task _cs4	= "stis$_cs4.cl"
	task _cs11	= "stis$_cs11.cl"
	task _cs12	= "stis$_cs12.cl"
	task normspflat	= "stis$normspflat.cl"
	task prepspec	= "stis$prepspec.cl"
	task mkfringeflat	= "stis$mkfringeflat.cl"
	task defringe	= "stis$defringe.cl"
	task daydark	= "stis$daydark.cl"
	task treqxy	= "stis$trxyeq/x_treqxy.e"
	task trxyeq	= "stis$trxyeq/x_trxyeq.e"
	task ucrpix	= "stis$trxyeq/x_ucrpix.e"

        hidetask echscript

print "The tasks in this package that run calstis and its modules (calstis,"
print "basic2d, ocrreject, wavecal, x1d, x2d) will no longer be supported."
print "All these tasks are available (with nearly the same parameters) in"
print "the Python/PyRAF stistools package, and the stistools interface runs"
print "an IRAF-independent version of calstis.  The IRAF-dependent version"
print "of calstis (the version run by the STSDAS stis package) will not be"
print "maintained, and it will be removed from STSDAS entirely in some"
print "future release."

        # set up stisnoise as a IRAF task
        pyexecute("stis$stisnoise_iraf.py",tasknames="stisnoise")

        # set up sshift as a IRAF task
        pyexecute("stis$sshift_iraf.py",tasknames="sshift")

        # set up wx2d as a IRAF task
        pyexecute ("stis$wx2d_iraf.py", tasknames="wx2d")

        # set up mktrace as a IRAF task
	pyexecute("stis$mktrace_iraf.py",tasknames="mktrace")

	cl()
end
