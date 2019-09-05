procedure ymkmu (input, output)

string	input {prompt="Input file name template"}
string	output {prompt="Output file name template"}

string	wave {"",prompt="Wavelength file name template"}
string	function {"legendre",min="|legendre|chebyshev|spline3|spline1|table",prompt="Function to fit to the wavelengths"}
int	nterms {4,min=1,prompt="Number of terms or spline pieces to fit with"}
string	label {"Wavelength",prompt="Label for the MULTISPEC system"}
string	units {"Angstroms",prompt="Units of the MULTISPEC system"}
real	low_reject {2.0,min=0.0,prompt="Low rejection in sigma of fit"}
real	high_reject {4.0,min=0.0,prompt="High rejection in sigma of fit"}
int	niterate {0,min=0,prompt="Number of rejection iterations"}
real	grow {1.,min=0.,prompt="Rejection growing radius in pixels"}
string	format {"%g",prompt="Format to use in writing the coefficients"}
bool	interactive {no,prompt="Interactively perform the fitting?"}
string	device {"stdgraph",prompt="Graphics device to use for interactive fitting"}
bool	markrej {yes,prompt="Mark rejected points?"}
bool	verbose {yes,prompt="Write out information messages?"}
string	Version {"17May00",prompt="Date of Installation"}

struct	*w_list

begin
	# Declarations
	string	fname
	string	in_list_file
	string	out_list_file
	string	pinput
	string	poutput
	string	tmproot
	string	w_list_file
	string	wt_list_file
	string	wtname

	# Make sure packages are loaded.
	fname = ""
	if (!defpac ("system"))
	    fname = fname//"system "
	if (!defpac ("imgtools"))
	    fname = fname//"imgtools "
	if (!defpac ("ctools"))
	    fname = fname//"ctools "
	if (fname != "")
	    error (1, "ymkmu: Please load packages: "//fname)
	
	# Get interactive parameters.
	pinput = input
	poutput = output

	# Make temporary file names.
	tmproot = mktemp ("tmp$YMKMU_")
	in_list_file = tmproot//"_ilf.txt"
	out_list_file = tmproot//"_olf.txt"
	w_list_file = tmproot//"_wlf.txt"
	wt_list_file = tmproot//"_wtlf.txt"

	# Make a file list of the input and output files.
	files (pinput, sort=no, > in_list_file)
	files (poutput, sort=no, > out_list_file )
	
	# Make a file list of the wavelengths.  This is necessary to
	# create the weight files.
	if (verbose)
	    print ("ymkmu: Making wavelength weight files...")
	if (wave == "") {
	    w_list = in_list_file
	    while (fscan (w_list, fname) != EOF) {
		fparse (fname, verbose=no)
		print (fparse.directory//fparse.root//".c0h", >> w_list_file)
	    }
	    w_list = ""
	} else {
	    files (wave, sort=yes, > w_list_file)
	}

	# Loop through all the wavelengths, creating an appropriate
	# weight file.
	w_list = w_list_file
	while (fscan (w_list, fname) != EOF) {
	    wtname = tmproot//"_wt.hhh"
	    if (verbose)
		printf ("%s: ", fname)
	    imcalc (fname, wtname, "if im1 > 0. then 1 else 0",
		    pixtype="old", nullval=0., verbose=verbose)
	    print (wtname, >> wt_list_file)
	}
	w_list = ""

	# Now run mkmultispec with the appropriate weights.
	if (verbose)
	    print ("ymkmu: Running mkmultispec...")
	mkmultispec ("@"//in_list_file, "@"//w_list_file, "@"//out_list_file,
		     function=function,
		     nterms=nterms, weight="@"//wt_list_file,
		     label=label, units=units, low_reject=low_reject,
		     high_reject=high_reject, niterate=niterate,
		     grow=grow, format=format, interactive=interactive,
		     device=device, markrej=markrej, cursor="",
		     verbose=verbose)

	# That's all folks.
	delete (tmproot//"*", verify=no)
end
