# This task reads one or more input spectra (tables), splices them
# together into a temporary file, and then fits a continuum to the
# spliced spectrum.  The result is a table of smoothed flux vs wavelength
# that can be used as input to the fweight task (fweight.intable2) for
# generating a column of weights for the splice task.
#
# Phil Hodge, 2000 Feb 22

procedure sflux (intable, outtable)

string intable = "" {prompt = "table(s) to be spliced together"}
string outtable = "" {prompt = "name of table for continuum fit"}
string logfile = "" {prompt = "list of log files for continuum fit"}
string function = "spline3" {prompt = "fitting function",
			min = "spline3|legendre|chebyshev|spline1"}
int order = 1 {prompt = "'order' of fitting function", min = 1}
bool interactive = no {prompt = "interactive fit for continuum?"}
bool verbose = yes {prompt = "print info?"}
string Version = "26Jun2000" {prompt = "date of installation"}
string mode = "al"

begin
	string intbl, outtbl	# local variables for intable, outtable
	string temp
	string incl_pweight	# copy of intable, with weights from pweight
	string spl_input	# input name for splice
	string spliced_tbl	# temp spliced spectrum
	string spliced_ms	# spliced_tbl converted to multispec format
	string spliced_c	# continuum fit in multispec format
	int nextend		# number of extensions in output from pweight
	bool tasks_found
	string junk		# for output from scan
	string datatype		# for data type of wavelength column

	tasks_found = yes			# initial value
	if (!deftask ("splice")) {
	    tasks_found = no
	    print "'stis' package is not loaded"
	}
	if (!deftask ("tomultispec")) {
	    tasks_found = no
	    print "'hst_calib' package is not loaded"
	}
	if (!deftask ("continuum")) {
	    tasks_found = no
	    print "'noao.onedspec' package is not loaded"
	}
	if (!deftask ("imtab")) {
	    tasks_found = no
	    print "'ttools' package is not loaded"
	}
	if (!deftask ("imdelete")) {
	    tasks_found = no
	    print "'images' package is not loaded"
	}
	if (!tasks_found) {
	    error (1, "please load packages")
	}

	intbl = intable
	outtbl = outtable

	if (access (outtbl)) {
	    error (1, "Output file already exists")
	}

	temp = mktemp ("tmp$sflux")		# ".lis" can be appended below
	incl_pweight = temp // "_pw.fits"
	spliced_ms = temp // "_ms.imh"
	spliced_c = temp // "_c.imh"
	spliced_tbl = temp // ".tab"

	if (verbose) {
	    print "running pweight ..."
	}
	pweight (intbl, incl_pweight, wgt_name="pweight", verbose=verbose)

	# The purpose of this statement is to reset nscan() to 1.
	print ("1") | scan (nextend)

	# If intable includes more than one table, incl_pweight will have
	# more than one extension.  Construct a list of all of the extensions
	# in incl_pweight, and run splice on that list.

	hedit (incl_pweight // "[0]", "nextend", ".",
		add=no, delete=no, verify=no, show=yes, update=no) |
			scan (junk, junk, nextend)
	if (nscan() < 3) {	# if keyword not found, nscan() will still be 1
	    nextend = 1
	}
	if (nextend > 1) {
	    temp = temp // ".lis"
	    for (i = 1;  i <= nextend;  i = i + 1) {
		print (incl_pweight // "[" + i // "]", >> temp)
	    }
	    spl_input = "@" // temp
	} else {
	    spl_input = incl_pweight
	}

	if (verbose) {
	    print "running splice ..."
	}
	splice (spl_input, spliced_tbl,
		wavetab="", spacing="coarse", sdqflags=32767,
		wl_name="wavelength", flux_name="flux", err_name="error",
		dq_name="dq",
		wgt_name="pweight", sw_name="",
		n_name="nelem", verbose=verbose)

	# To find out whether the spliced table contains arrays or scalars.
	# write the data type of the wavelength column into datatype, then
	# check for a value similar to "D[1024]".
	datatype = "nothing"
	tlcol (spliced_tbl, nlist=2) |
		match ("{wavelength}", print=no, metachar=yes) |
		scan (junk, datatype)
	if (datatype == "nothing") {
	    error (1, "WAVELENGTH column not found")
	}

	if (strlen (datatype) > 1 && substr (datatype, 2, 2) == "[") {

	    # The table contains array columns.
	    if (verbose) {
		print "running tomultispec ..."
	    }
	    tomultispec (spliced_tbl, spliced_ms,
		flux_col="FLUX", wave_col="WAVELENGTH",
		function="chebyshev", nterms=4,
		label="Wavelength", units="Angstroms",
		low_reject=2., high_reject=4., niterate=0, grow=1.,
		weight="", format="%g", interactive=no,
		device="stdgraph", markrej=yes, verbose=verbose,
		tempdir="tmp$", dout_list="")

	} else {

	    # The table contains scalar columns.
	    if (verbose) {
		print "running mkmultispec ..."
	    }
	    tabim (spliced_tbl, spliced_ms, "FLUX", 0, n1=1)

	    # Operate on spliced_ms in-place.
	    mkmultispec (spliced_ms, spliced_tbl // "[WAVELENGTH]", "",
		function="chebyshev", nterms=4, weight="",
		label="Wavelength", units="Angstroms",
		low_reject=2., high_reject=4., niterate=0, grow=1.,
		format="%g", interactive=no,
		device="stdgraph", markrej=yes, cursor="", verbose=verbose)
	}

	if (verbose) {
	    print "running continuum ..."
	}
	continuum (spliced_ms, spliced_c,
		"yes", lines="*", bands="1", type="fit",
		replace=no, wavescale=yes, logscale=no, override=no,
		listonly=no,
		logfiles=logfile, interactive=interactive,
		sample="*", naverage=1,
		function=function, order=order,
		low_reject=2., high_reject=0., niterate=10, grow=1.,
		markrej=yes, graphics="stdgraph", cursor="")

	if (verbose) {
	    print "running imtab ..."
	}
	imtab (spliced_c, outtbl,
		"flux", pname="wl", wcs="world", formats="", tbltype="default")

	tchcol (outtbl, "wl1", "wavelength", "", "", verbose=no)

	# delete temporary files
	if (nextend > 1) {
	    delete (temp)
	}
	delete (incl_pweight, verify=no)
	delete (spliced_tbl, verify=no)
	imdelete (spliced_ms, verify=no)
	imdelete (spliced_c, verify=no)
end
