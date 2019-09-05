# cl wrapper for calstis2
# R. Katsanis 4Jun97
# Modified by R. Katsanis on 16Jan98 to include modifications/updates done
# to calstis-2.
#
procedure ocrreject (input,output)

file    input    	{"",prompt="Input STIS file name(s)"}
file	output	 	{"",prompt="Name of output file"}
bool	all		{yes,prompt="Combine all input files into 1 file?"}
file    crrejtab 	{"",prompt="Reference file name"}
file	scalense	{"",prompt="Scale factor applied to noise"}
file    initgues	{"",prompt="Initial value estimate scheme (min|med)"}
file	skysub		{"",prompt="How to compute the sky (none|mode)"}
file	crsigmas	{"",prompt="Rejections levels in each iteration"}
real    crradius	{INDEF,prompt="CR expansion radius in pixels"}
real    crthresh        {INDEF,prompt="Rejection propagation threshold"}
int	badinpdq        {INDEF,prompt="Data quality flag bits to reject"}
file    crmask          {"",prompt="Flag CR in input DQ images (yes|no)? "}
bool    verbose  	{yes, prompt="Print additional info?"}
string  Version         {"3.4 (13-Nov-2013)", prompt="calstis version"}
struct  *olist

begin

	# local variables
	string inp, inp1, inp2, out, out1, crfile, init, skymode, sigma, inlist
	string outlist, noise, mask
	real    radius, thresh
	int     dq, numinput, numoutput	
	string  crrejstr, crrejstr1, crrejstr2, crrejstr3, root


	# get input parameters from par files
	  inp = input       ;      out = output    ;   crfile = crrejtab
	 init = initgues    ;  skymode = skysub    ;    sigma = crsigmas
	noise = scalense    ;   radius = crradius  ;   thresh = crthresh
	   dq = badinpdq    ;     mask = crmask


	# Check values of some input parameters.
	if ( init != "min" && init != "med" && init != "" )
	     error (1, "Input parameter 'initgues' out of range")
	if ( skymode != "none" && skymode != "mode" && skymode != "" )
	     error (1, "Input parameter 'skysub' out of range")
	if ( mask != "yes" && mask != "no" && mask != "" )
	     error (1, "Input parameter 'crmask' out of range")

	# Build general command strings
	# cs2.e should be on $PATH
	crrejstr1 = "!cs2.e "
	crrejstr3 = ""
	if (verbose)
	    crrejstr3 = crrejstr3 // " -t -v "
	if (crfile != "" )
	    crrejstr3 = crrejstr3 // " -table '" // crfile // "'"
	if (noise != "")
	    crrejstr3 = crrejstr3 // " -scale " // noise
	if (init != "")
	    crrejstr3 = crrejstr3 // " -init " // init
	if (skymode != "")
	    crrejstr3 = crrejstr3 // " -sky " // skymode
	if (sigma != "")
	    crrejstr3 = crrejstr3 // " -sigmas " // sigma
	if (radius != INDEF)
	    crrejstr3 = crrejstr3 // " -radius " // radius
	if (thresh != INDEF)
	    crrejstr3 = crrejstr3 // " -thresh " // thresh
	if (dq != INDEF)
	    crrejstr3 = crrejstr3 // " -pdq " // dq
	if (mask != "" )
	     crrejstr3 = crrejstr3 // " -crmask " // mask

	# if combine all input files into 1 output file.
	if ( all )  {
	     # define inp tring in case it contains a wild card.
	     inp2 = "\""//inp//"\""
	     # if output filename is NULL.
	     if ( out == "" )  {
		  inlist = mktemp("tmp$inlist")
		  sections (inp, option="fullname", > inlist)
		  list = inlist
		  j = fscan(list,inp1) ; printf ("inp1=%s\n", inp1)
		  delete (inlist, yes, verify=no)
	          if ( substr(inp1,strlen(inp1)-4,strlen(inp1)-4) == "." )
		       root = substr(inp1,1,strlen(inp1)-5)
	          else if ( substr(inp1,strlen(inp1)-3,strlen(inp1)-3) == "." )
		            root = substr(inp1,1,strlen(inp1)-4)
	          else  root = inp1
		  out = root // "_crj.fits"
		  crrejstr2 = inp2 // " " // out
		  crrejstr = crrejstr1 // crrejstr2 // crrejstr3
		  printf  ("%s\n",crrejstr)   # display the string only
		  print (crrejstr) | cl      # execute it
		  printf ("\n")
	     }
	     else {# if input & output filenames are given.
		    crrejstr2 = inp2 // " " // out
		    crrejstr = crrejstr1 // crrejstr2 // crrejstr3
		    printf  ("%s\n",crrejstr)   # display the string only
		    print (crrejstr) | cl  	# execute it
		    printf ("\n")
	     }
	}

	else  {
		# if output filename is NULL.
		if ( out == "" )  {
		     inlist = mktemp("tmp$inlist")
		     sections (inp, option="fullname", > inlist)
		     list = inlist
		     while ( fscan(list,inp1) != EOF )  {
			     if ( substr(inp1,strlen(inp1)-4,strlen(inp1)-4) == "." )
				  root = substr(inp1,1,strlen(inp1)-5)
	                     else if ( substr(inp1,strlen(inp1)-3,strlen(inp1)-3) == "." )
		                       root = substr(inp1,1,strlen(inp1)-4)	
	                     else root = inp1
		             out1 = root // "_crj.fits"
			     crrejstr2 = inp1 // " " // out1
		  	     crrejstr = crrejstr1 // crrejstr2 // crrejstr3
		 	     printf  ("%s\n",crrejstr)  # display the string only
		  	     print (crrejstr) | cl  	# execute it
		  	     printf ("\n")
	      	     }
		     delete (inlist, yes, ver-)
		 }
		 else  {# if input & output filenames are given.
	         	inlist = mktemp("tmp$inlist")
		 	sections (inp, option="fullname", > inlist)
		 	outlist = mktemp("tmp$olist")
		 	sections (out, option="fullname", > outlist)
		 	count ( inlist ) | scan (numinput)
		 	count ( outlist ) | scan (numoutput)
		 	if ( numinput != numoutput )
		      	     error (1, "Number of input and output files do not match.")
		 	else  {
				list = inlist ; olist = outlist
				while ( fscan(list,inp1) != EOF )  {
					j = fscan (olist,out1)
					crrejstr2 = inp1 // " " // out1
		  	        	crrejstr = crrejstr1 // crrejstr2 // crrejstr3
		 	        	printf  ("%s\n",crrejstr)  # display the string only
		  	        	print (crrejstr) | cl     # execute it
		  	        	printf ("\n")
		        	}
				delete (inlist//","//outlist, yes, ver-)
		 	}
	        }
	}
end
