# FWPLOT -- Plot calibrated spectroscopic data
#
# Plot one group of calibrated flux data versus one group
# of calibrated wavelength data. Overplotting of propagated
# errors as error bars is permitted. If given the flux image
# name only, construct the image names for the wavelength
# and error images assuming the standard extensions for the
# HRS and FOS GEIS sets, i.e., c0h = wavelength; c1h = flux;
# and c2h = propagated errors.

procedure fwplot (flux)

file 	flux	{prompt="calibrated flux image"}
file 	wave	{"",prompt="calibrated wave image"}
file 	err	{"",prompt="calibrated propagated error image"}
char	title	{"",prompt="title"}
char	device	{"stdgraph",prompt="Graphics device"}
char	xlabel	{"WAVELENGTH",prompt="xlabel"}
char	ylabel	{"FLUX",prompt="ylabel"}
bool	plterr	{no,prompt="plot error bars?"}

pset	dvpar	{"",prompt="Device parameters"}
pset	pltpar	{"",prompt="Plot attributes"}
pset	axispar {"",prompt="Scaling attributes"}

begin
        
        file	temp0, temp1, temp2, temp3
        char	tlabel, dev, inp
        int	len, i, iext
        
        # Check for packages loaded.
        temp0 = ""
	if (!defpac ("stplot"))
            temp0 = temp0//"stplot "
	if (!defpac ("ttools"))
            temp0 = temp0//"ttools "
	if (!defpac ("tools"))
	    temp0 = temp0//"tools "
        if (strlen (temp0) > 0)
            error (1, "fwplot: Please load packages: "//temp0)
        
	# get the input images
	temp1 = flux 
	temp2 = wave 
	temp3 = err
        
	# get the title
	if (title == "")
	    tlabel = temp1
	else
	    tlabel = title
        
	# make a temporary name for the temporary table
	temp0 = mktemp("tmp$fwplot")
        
	# Build wavelength and error image name if none given.
	# Assumes flux has 'c?h' extension and wavelength
	# has 'c0h' extension and error has 'c2h' extension.
	fparse (temp1, verbose=no)
	if (temp2 == ""){
	    temp2 = fparse.directory//fparse.root//".c0h"
	    if (fparse.cl_index > 0)
		temp2 = temp2//"["//fparse.cl_index//"]"
	    temp2 = temp2//fparse.section//fparse.ksection
	}
	if (plterr && temp3 == "") {
	    temp3 = fparse.directory//fparse.root//".c2h"
	    if (fparse.cl_index > 0)
		temp3 = temp3//"["//fparse.cl_index//"]"
	    temp3 = temp3//fparse.section//fparse.ksection
	}

	# build the temporary table
	imtab (input=temp1,outtable=temp0,colname="flux",
	       pname = "", tbltype = "default")
	imtab (input=temp2,outtable=temp0,colname="wavelength",
	       pname = "", tbltype = "default")
	if (plterr) 
	    imtab (input=temp3,outtable=temp0,colname="error",
		   pname = "", tbltype = "default")
	
	# construct the input string to feed to sgraph
	inp = temp0 + " wavelength flux"
	
	# plot the data values
	sgraph (input=inp, errcolumn="", xlabel=xlabel, device=device,
		ylabel=ylabel, title=tlabel)
	
	# if required plot the error bars
	if (plterr) 
	    sgraph (input=inp, errcolumn="error", append = yes, erraxis=2,
		    device=device)
	
	# delete the temporary table
	tdelete (temp0, yes, verify=no, default_action=yes)
	
end
