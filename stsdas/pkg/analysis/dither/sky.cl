# CGSKY  --  Corrected gsky.
#
#  This calls the original gsky and corrects the resulting sky measurement
#  from histogram truncation effects
#
#  26-Mar-97:  Task created (I. Busko)
#  21-Aug-97:  Directory for temporary files (IB)
#  27-Oct-97:  Renamed 'sky'. The former gsky task was renamed 'ogsky' and
#              hidden (IB)
#  11-Nov-97:  Checks if sky-subtracted by crrej (IB)
#  09-Feb-98:  New design (IB)
#  17-Apr-98:  Report all groups in no-subtract, no-verbose mode (IB)
#  20-Oct-98:  Truncate string passed to imcalc (IB)
#  25-Feb-99:  Add expname to process countrate images (JC Hsu)
#  19-May-99:  Add bunit to process countrate images (JC Hsu)
#  15-Jun-99:  change width from int to real (JC Hsu)
#  03-Feb-2000: Initialize kwrd and spell out wkstr's (JC Hsu)
#  24-Jul-2002: Corrected the imcalc 'str(pval)' error (WJ Hack)

procedure sky (input)

char	input    = ""      {prompt="Input images"}
char	masks    = ""      {prompt="Input masks"}
real	lower    = -99.    {prompt="Lower limit of usable data (always in DN)"}
real	upper    = 4096.   {prompt="Upper limit of usable data (always in DN)"}
pset	dq                 {prompt="Data Quality bits"}
bool	subsky   = no      {prompt="Subtract sky from input images ?"}
real	width    = 8       {prompt="Interval width for sky statistics (in same unit as the image)",min=0.}
char	stat     = "mode"  {prompt="Sky correction statistics",\
                            enum="mean|mode"}
char	bunit    = "      " {prompt="is the image in count or count per second?",				enum="|      |cps|counts"}
char	expname  = ""	   {prompt="Exposure time keyword name, for countrate images ONLY"}
char	skyname  ="BACKGRND"{prompt="Header parameter to be updated with sky"}
real	skyvalue = 0.      {prompt="Last image/group sky value (output)"}
bool	verbose  = no      {prompt="Print out verbose messages ?"}
file	tempdir  = "tmp$"  {prompt="Directory for temporary files"}
char	version  = "24Jul2002" {prompt="Date of installation"}

begin
	# These hold input parameters.
	char t_input, t_masks, t_bunit, t_expname, t_skyname, t_stat, t_tempdir
	real t_lower, t_upper
	real t_width
	bool t_subsk, t_verb

	# These are for internal use.
	file image, tempfile, temp1, temp2, temp3
	char h_skyname, subk, msg, wkstr
	int  f1, group, igroup, gcount
	real pval, t_skyval, n_skyval
	struct kwrd

	# These are for saving the called tasks' parameters.
	char k_masks, k_bunit, k_expname, k_skyname
	char s_masks, s_groups, s_fields
	char m_pixt
	real k_lower, k_upper
	real s_lower, s_upper
	real m_null
	bool k_subsk, k_verb
	bool s_gaccum
	bool m_verb

	# Check for the presence of pre-requisite tasks/packages.
	msg = ""
	if (!deftask("pickfile")) msg = msg // " imgtools"
	if (strlen(msg) > 0) {
	    printf ("Please, load packages: %s\n", msg)
	    bye
	}

	# Read task parameters.
	t_input   = input
	t_masks   = masks
	t_lower   = lower
	t_upper   = upper
	t_subsk   = subsky
	t_width   = width
	t_stat    = stat
	t_bunit   = bunit
	t_expname = expname
	t_skyname = skyname
	t_verb    = verbose
	t_tempdir = tempdir

	# Save called tasks' parameters.
	k_masks   = ogsky.masks
	k_lower   = ogsky.lower
	k_upper   = ogsky.upper
	k_subsk   = ogsky.subsky
	k_bunit   = ogsky.bunit
	k_expname = ogsky.expname
	k_skyname = ogsky.skyname
	k_verb    = ogsky.verbose
	s_masks   = gstatistics.masks
	s_groups  = gstatistics.groups
	s_fields  = gstatistics.fields
	s_lower   = gstatistics.lower
	s_upper   = gstatistics.upper
	s_gaccum  = gstatistics.g_accum
	m_pixt    = imcalc.pixtype
	m_null    = imcalc.nullval
	m_verb    = imcalc.verbose

	n_skyval = 0.

	# Set called tasks' parameters.
	ogsky.masks   = t_masks
	ogsky.lower   = t_lower
	ogsky.upper   = t_upper
	ogsky.subsky  = t_subsk
	ogsky.bunit   = t_bunit
	ogsky.expname = t_expname
	ogsky.skyname = t_skyname
	ogsky.verbose = t_verb
	gstat.masks   = t_masks
	gstat.groups  = "*"
	gstat.g_accum = no
	gstat.fields  = t_stat
	gstat.lower   = - t_width / 2.
	gstat.upper   =   t_width / 2.
	imcalc.pixtype =  "real"
	imcalc.nullval = 0.
	imcalc.verbose = t_verb

	# Create temporary image names.
	tempfile = t_tempdir // "gsky"
	temp1  = mktemp (tempfile)
	temp2  = mktemp (tempfile)
	temp3  = mktemp (tempfile)

	# Main loop. Scan file list and process each in turn.
	gcount = 0
	countfile (t_input)
        for (f1 = 1; f1 <= countfile.output; f1+=1) {
	    pickfile (t_input, f1)
	    image = pickfile.output

	    # Check group structure.
	    hselect (image, "gcount", "yes") | scan (gcount)
            if (gcount == 0)
                gcount = 1
	    if (gcount != 4 && gcount != 1)
	        error (1, "Unexpected number of groups in image.")

	    # Check if sky was already subtracted by this task.
	    subk = ""
	    hselect (image, "SUBSKY", "yes") | scan (subk)

	    # Check if sky was subtracted by crrej. This is potentialy
            # dangerous because the information wheter the sky was
            # subtracted is in an HISTORY record. HISTORY records
            # shouldn't be used to control processing...
	    kwrd = ""
	    if (subk != "T") {
	        match ("HISTORY   Sky used", image) | scan (kwrd)
	        if (substr (kwrd, 1, 18) == "HISTORY   Sky used") {
	            if (substr (kwrd, 19, 25) == " : mode")
	                subk = "T"
	        }
	    }

	    if (!t_verb) {
                if (countfile.output > 1)
	        print ("# ", image)
	        print ("# Group    Rough sky   Delta sky    Final sky")
	        print ("#")
	    }

	    # Different procedures depending if we want the sky
            # subtracted or not.
	    if (t_subsk) {

	        # Process each group.
	        for (group = 1;  group <= gcount;  group += 1) {
	            if (gcount == 1)
	                wkstr = ""
	            else 
	                wkstr = "[" // str(group) // "]"

                    if (subk == "T") {

	                # Sky was already subtracted; look for background
                        # keyword in header.
	                imgets (image // wkstr, t_skyname, >& "dev$null")
	                if (imgets.value != "0") {
	                    t_skyval = real (imgets.value)
	                } else {
	                    print (\
                            "Warning: Sky has been previously subtracted, ")
	                    print (\
                            "         but sky value header keyword not found")
	                    break
	                }
                    } else {

	                # Sky was not previously subtracted. Proceed
                        # with full subtraction. ogsky can only write
                        # the background keyword if run on all groups
                        # at once.
	                if (group == 1)
	                    ogsky (image)

	                # Read sky value for this group.
	                imgets (image // wkstr, t_skyname, >& "dev$null")
	                t_skyval = real (imgets.value)
	            }

	            # Compute the offset correction factor and add 
                    # it to previously computed sky level.
	            gstatistics (image // wkstr, > "dev$null")
	            if (t_stat == "mean") {
	                pval = gstpar.mean
	                n_skyval = t_skyval + pval
	            } else if (t_stat == "mode") {
	                pval = gstpar.mode
	                n_skyval = t_skyval + pval
	            } else {
	                pval = INDEF
	                t_skyval = INDEF
	                n_skyval = INDEF
	            }
                
	            # Subtract the offset correction factor. The string
	            if (t_verb)
	                printf ("Subtracting %g from group %d ...\n", 
                                 pval, group)
	            imcalc (image // wkstr, image // wkstr,
                           "im1 - " // str(pval), verb=t_verb)

	            # Update or write keyword with new sky value.
	            hedit (image // wkstr, t_skyname, n_skyval, add+, \
                           delete-, verify-, show=t_verb, update+)

	            # Report results.
	            if (!t_verb)
	                printf ("   %d      %9g    %9g    %9g \n",\
                                group, t_skyval, pval, n_skyval)
	            else
	                printf (\
  "Group: %d   Rough sky: %9g   Delta sky: %9g   Final sky: %9g \n",\
                                group, t_skyval, pval, n_skyval)

	            # Report final sky value into CL parameter.
	            skyvalue = n_skyval
	        }

	    } else {

	        # If non-verbose, set the group loop to scan only the 
                # last group in this file.
#	        if (t_verb)
	            igroup = 1
#	        else
#	            igroup = gcount

	        # Process each group.
	        for (group = igroup;  group <= gcount;  group += 1) {
	            if (gcount == 1)
	                wkstr = ""
	            else 
	                wkstr = "[" // str(group) // "]"

                    if (subk == "T") {

	                # Sky was already subtracted; look for background
                        # keyword in header.
	                imgets (image // wkstr, t_skyname, >& "dev$null")
	                if (imgets.value != "0") {
	                    t_skyval = real (imgets.value)

	                    # Compute the offset correction factor and add 
                            # it to previously computed sky level.
	                    gstatistics (image // wkstr, > "dev$null")
	                    if (t_stat == "mean") {
	                        pval = gstpar.mean
	                        n_skyval = t_skyval + pval
	                    } else if (t_stat == "mode") {
	                        pval = gstpar.mode
	                        n_skyval = t_skyval + pval
	                    } else {
	                        pval = INDEF
	                        t_skyval = INDEF
	                        n_skyval = INDEF
	                    }
	                } else {
	                    print (\
                            "Warning: Sky has been previously subtracted, ")
	                    print (\
                            "         but sky value header keyword not found")
	                    break
	                }
	            } else {

	                # Sky was not subtracted yet. We must subtract it
                        # anyway, in order to apply the correction algorithm 
                        # in the result. So we do it in a temporary image.
	                imcopy (image // wkstr, temp1, > "dev$null")
	                ogsky (temp1, subsky+, > "dev$null")
	                t_skyval = ogsky.skyvalue

	                # Compute the offset correction factor and add 
                        # it to previously computed sky level.
	                gstatistics (temp1, > "dev$null")
	                if (t_stat == "mean") {
	                    pval = gstpar.mean
	                    n_skyval = t_skyval + pval
	                } else if (t_stat == "mode") {
	                    pval = gstpar.mode
	                    n_skyval = t_skyval + pval
	                } else {
	                    pval = INDEF
	                    t_skyval = INDEF
	                    n_skyval = INDEF
	                }
	                imdelete (temp1, verify-, >& "dev$null")
	            }

	            # Report results.
	            if (!t_verb)
	                printf ("   %d      %9g    %9g    %9g \n",\
                                group, t_skyval, pval, n_skyval)
	            else
	                printf (\
  "Group: %d   Rough sky: %9g   Delta sky: %9g   Final sky: %9g \n",\
                                group, t_skyval, pval, n_skyval)

	            # Report final sky value into CL parameter.
	            skyvalue = n_skyval
	        }
	    }
	}

	# Recover called tasks' parameters.
	ogsky.masks    = k_masks
	ogsky.lower    = k_lower
	ogsky.upper    = k_upper
	ogsky.subsky   = k_subsk
	ogsky.bunit    = k_bunit
	ogsky.expname  = k_expname
	ogsky.skyname  = k_skyname
	ogsky.verbose  = k_verb
	gstatistics.masks    = s_masks
	gstatistics.groups   = s_groups
	gstatistics.fields   = s_fields
	gstatistics.lower    = s_lower
	gstatistics.upper    = s_upper
	gstatistics.g_accum  = s_gaccum
	imcalc.pixtype = m_pixt
	imcalc.nullval = m_null
	imcalc.verbose = m_verb
end
