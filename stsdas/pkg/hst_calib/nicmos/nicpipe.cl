procedure nicpipe (input, output, stage)

# Runs NICMOS MultiAccum data through various steps of "calnica".
#
# If preparing data for input to the "biaseq" task the steps ZSIGCORR,
# ZOFFCORR, MASKCORR, BIASCORR, NOISCALC, DARKCORR, NLINCORR, BARSCORR,
# PHOTCALC, BACKCALC, and WARNCALC are performed.
#
# For final processing all remaining unperformed steps are performed
# (usually FLATCORR, UNITCORR, and CRIDCALC). If processing a Grism image,
# FLATCORR is omitted.
#
#
# Revision history:
#
# Initial public release: Howard Bushouse, 5/99 - Written based on "nicpipe1"
#	and "nicpipe2" scripts by Mark Dickinson, 4/98.
#
# STSDAS v2.1 release: H.Bushouse, 9/99 - Added check for Grism images to
#	turn off FLATCORR; Do all previously unperformed steps for
#	"final" stage in case the "pedsky" stage is ever skipped.
#
# 09 February 2000: H.Bushouse - Added "flprcache" after setting imtype
#
# 01 June 2000: H.Bushouse - Removed "pedsky" option for "stage" parameter,
#	because pedsky task now accepts input images that have been
#	flatfielded.
#

string	input 		{prompt="Images to process"}
string	output 		{prompt="Output images"}
string	stage		{prompt="Processing stage: biaseq, final"}

struct	*inimglist
struct	*outimglist

begin

	string	t_inlist, t_outlist, t_stage
	string	inl, outl
 	string	img, outimg
	int	nin
	bool	outf

	# Get query parameters
	t_inlist  = input
	t_outlist = output
	t_stage   = stage

	if (t_stage != "biaseq" && t_stage != "final")
	    error (1, "Unrecognized value specified for stage parameter")

	# Load necessary packages
	if (!defpac("images")) images
	if (!defpac("imutil")) imutil
	if (!defpac("stsdas")) stsdas motd-
	if (!defpac("hst_calib")) hst_calib
	if (!defpac("nicmos")) nicmos
	set imtype = "fits"
	flprcache

	# Make a temporary list of the input image names
	inl = mktemp ("tmp$nicpipe")
	sections (t_inlist,option="fullname",>inl)
	nin = sections.nimages
	inimglist = inl

	# Make a temporary list of the output image names
	if (strlen(t_outlist) > 0) {
	    outl = mktemp ("tmp$nicpipe")
	    sections (t_outlist,option="fullname",>outl)
	    if (sections.nimages != nin) 
		error (1,"Different numbers of input and output images")
	    outimglist = outl
	    outf = yes
	} else {
	    outf = no
	}
		
	# Loop over list of input images
	while (fscan(inimglist,img) != EOF) {

	   # Set the calibration switches in the input images
	   if (t_stage == "biaseq") {
	       hedit (img//"[0]", "ZSIGCORR", "PERFORM", add+, delete-, verify-,
		      show-, updat+)
	       hedit (img//"[0]", "ZOFFCORR", "PERFORM", add-, delete-, verify-,
		      show-, updat+)
	       hedit (img//"[0]", "MASKCORR", "PERFORM", add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "BIASCORR", "PERFORM", add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "NOISCALC", "PERFORM", add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "DARKCORR", "PERFORM", add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "NLINCORR", "PERFORM", add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "BARSCORR", "PERFORM", add+, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "FLATCORR", "OMIT",    add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "UNITCORR", "OMIT",    add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "CRIDCALC", "OMIT",    add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "PHOTCALC", "PERFORM", add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "BACKCALC", "PERFORM", add-, delete-, verify-,		      show-, updat+)
	       hedit (img//"[0]", "WARNCALC", "PERFORM", add-, delete-, verify-,		      show-, updat+)

	   } else if (t_stage == "final") {

	       imgets (img//"[0]", "ZSIGDONE")
	       if (imgets.value != "PERFORMED")
		   hedit (img//"[0]", "ZSIGCORR", "PERFORM", add+, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "ZSIGCORR", "OMIT",    add+, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "ZOFFDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "ZOFFCORR", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "ZOFFCORR", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "MASKDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "MASKCORR", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "MASKCORR", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "BIASDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "BIASCORR", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "BIASCORR", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "NOISDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "NOISCALC", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "NOISCALC", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "DARKDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "DARKCORR", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "DARKCORR", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "NLINDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "NLINCORR", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "NLINCORR", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "BARSDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "BARSCORR", "PERFORM", add+, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "BARSCORR", "OMIT",    add+, delete-,
			  verif-, sho-, upd+)

	       imgets (img//"[0]", "FLATDONE")
	       if (imgets.value != "PERFORMED") {
		   imgets (img//"[0]", "FILTER")
		   if (imgets.value != "G096" && imgets.value != "G141" &&
		       imgets.value != "G206") {
		       hedit (img//"[0]", "FLATCORR", "PERFORM", add-, delete-,
			      verif-, sho-, upd+)
		   } else {
		       print ("\n",img,
			      " is a GRISM image; FLATCORR will be OMITTED\n")
		       hedit (img//"[0]", "FLATCORR", "OMIT",    add-, delete-,
			      verif-, sho-, upd+)
		   }
	       } else {
		   hedit (img//"[0]", "FLATCORR", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       }
	       imgets (img//"[0]", "UNITDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "UNITCORR", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "UNITCORR", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "CRIDDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "CRIDCALC", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       else {
		   hedit (img//"[0]", "CRIDCALC", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
		   hedit (img//"[0]", "NSAMP",         1,    add-, delete-,
			  verif-, show-, updat+)
		   hedit (img//"[0]", "OBSMODE", "ACCUM",    add-, delete-,
			  verif-, show-, updat+)
	       }
	       imgets (img//"[0]", "PHOTDONE")
	       if (imgets.value != "PERFORMED")
	           hedit (img//"[0]", "PHOTCALC", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       else
		   hedit (img//"[0]", "PHOTCALC", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "BACKDONE")
	       if (imgets.value == "OMITTED")
	           hedit (img//"[0]", "BACKCALC", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       else
	           hedit (img//"[0]", "BACKCALC", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	       imgets (img//"[0]", "WARNDONE")
	       if (imgets.value == "OMITTED")
	           hedit (img//"[0]", "WARNCALC", "OMIT",    add-, delete-,
			  verif-, sho-, upd+)
	       else
	           hedit (img//"[0]", "WARNCALC", "PERFORM", add-, delete-,
			  verif-, sho-, upd+)
	   }
 
	   # Run CALNICA
	   if (outf) {
	       if (fscan (outimglist, outimg) == EOF) 
		   error (1,"Problem reading output image list")
	       calnica (img, outimg)
	   } else {
	       calnica (img, "")
       	   }
	}

	# Delete temp files
	delete (inl, verify-, >& "dev$null")
	if (outf)
	    delete (outl, verify-, >& "dev$null")

	inimglist = ""
	outimglist = ""

end
