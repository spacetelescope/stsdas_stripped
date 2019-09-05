#
# CGSKY  --  Corrected gsky.
#
#  This calls the original gsky and corrects the resulting sky measurement
#  from histogram truncation effects
#
#
#
#  26-Mar-97:  Task created (I. Busko)
#  21-Aug-97:  Directory for temporary files (IB)

procedure cgsky (input)

char	input    = ""      {prompt=">Input images"}
char	masks    = ""      {prompt=">Input masks"}
real	lower    = -99.    {prompt=">Lower limit of usable data"}
real	upper    = 4096.   {prompt=">Upper limit of usable data"}
pset	dq       = ""      {prompt=">Data Quality bits"}
bool	subsky   = no      {prompt=">Subtract sky from input images ?"}
int	width    = 8       {prompt=">Interval width for sky statistics",min=2}
char	stat     = "mode"  {prompt=">Sky correction statistics",\
                            enum="mean|mode"}
char	skyname  = ""      {prompt=">Header parameter to be updated with sky"}
real	skyvalue = 0.      {prompt=">Last image/group sky value (output)"}
bool	verbose  = yes     {prompt=">Print out verbose messages ?"}
file	tempdir  = "tmp$"  {prompt=">Directory for temporary files"}
char	version  = "26Mar97" {prompt=">Date of installation"}

begin
	# These hold input parameters.
	char t_input, t_masks, t_skyname, t_stat, t_tempdir
	real t_lower, t_upper
	int  t_width
	bool t_subsk, t_verb

	# These are for internal use.
	file image, tempfile, temp1, temp2
	char h_skyname, subk, msg
	int  f1, group, gcount
	real pval, t_skyval

	# These are for saving the called tasks' parameters.
	char k_masks, k_skyname
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
	t_skyname = skyname
	t_verb    = verbose
	t_tempdir = tempdir

	# Save called tasks' parameters.
	k_masks   = gsky.masks
	k_lower   = gsky.lower
	k_upper   = gsky.upper
	k_subsk   = gsky.subsky
	k_skyname = gsky.skyname
	k_verb    = gsky.verbose
	s_masks   = gstatistics.masks
	s_groups  = gstatistics.groups
	s_fields  = gstatistics.fields
	s_lower   = gstatistics.lower
	s_upper   = gstatistics.upper
	s_gaccum  = gstatistics.g_accum
	m_pixt    = imcalc.pixtype
	m_null    = imcalc.nullval
	m_verb    = imcalc.verbose

	# Set called tasks' parameters.
	gsky.masks   = t_masks
	gsky.lower   = t_lower
	gsky.upper   = t_upper
	gsky.subsky  = t_subsk
	gsky.skyname = t_skyname
	gsky.verbose = t_verb
	gstat.masks   = t_masks
	gstat.groups  = "*"
	gstat.g_accum = no
	gstat.fields  = t_stat
	gstat.lower   = - t_width / 2
	gstat.upper   =   t_width / 2
	imcalc.pixtype =  "real"
	imcalc.nullval = 0.
	imcalc.verbose = t_verb

	# Create temporary image names.
	tempfile = t_tempdir // "cgsky"
	temp1  = mktemp (tempfile)
	temp2  = mktemp (tempfile)

	# Main loop. Scan file list and process each in turn.
	subk = ""
	gcount = 0
	countfile (t_input)
        for (f1 = 1; f1 <= countfile.output; f1+=1) {
	    pickfile (t_input, f1)
	    image = pickfile.output

	    # Check group structure.
	    hselect (image, "gcount", "yes") | scan (gcount)
	    if (gcount != 4 && gcount != 1)
	        error (1, "Unexpected number of groups in image.")

	    # Different procedures depending if we want the sky
            # subtracted or not.
	    if (t_subsk) {

	        # Check if sky was already subtracted.
	        hselect (image, "SUBSKY", "yes") | scan (subk)
                if (subk != "T") {
	            gsky (image)
	            # This stores the last group's sky only !
	            t_skyval = gsky.skyvalue
	        } else {
	            printf ("%s - SUBSKY = 'T', skipping sky subtraction...\n",
                            image)
	            t_skyval = 0.
	        }

	        # Process each group.
	        for (group = 1;  group <= gcount;  group += 1) {

	            # Compute the correction factor and add it to
                    # previously computed (by gsky) sky level.
	            gstatistics (image // "[" // str(group) // "]", > \
                                 "dev$null")
	            if (t_stat == "mean") {
	                pval = gstpar.mean
	                if (group == gcount)
	                    t_skyval = t_skyval + pval
	            } else if (t_stat == "mode") {
	                pval = gstpar.mode
	                if (group == gcount)
	                    t_skyval = t_skyval + pval
	            } else {
	                pval = INDEF
	                t_skyval = 0.
	            }

	            # Subtract the correction factor.
	            if (t_verb)
	                printf ("Subtracting %g from group %d ...\n", 
                                 pval, group)
	            imcalc (image // "[" // str(group) // "]", temp1,
                            "im1 - " // str(pval), verb=t_verb)
	            if (group == 1)
	                 imcopy (temp1, temp2 // "[1/" // str(gcount) // "]",
                                 > "dev$null")
	            else
	                imcopy (temp1, temp2 // "[" // str(group) // "]",
                                > "dev$null")
	            imdelete (temp1, verify-, >& "dev$null")
	        }
	        imdelete (image, verify-, >& "dev$null")
	        imrename (temp2, image, verbose-)

	    } else {

	        # Check if sky was already subtracted.
	        hselect (image, "subsky", "yes") | scan (subk)
                if (subk == "T")
	            error (1, "Cannot compute sky, it was already subtracted.")

	        # Process each group.
	        for (group = 1;  group <= gcount;  group += 1) {

	            # We must subtract the sky anyway, in order to apply
                    # the correction algorithm in the result. So we do it
                    # in a temporary image.
	            imcopy (image // "[" // str(group) // "]", temp1,
                            > "dev$null")

	            # Now subtract the sky and compute correction.
	            gsky (temp1, subsky+, > "dev$null")
	            t_skyval = gsky.skyvalue
	            gstatistics (temp1, > "dev$null")
	            if (t_stat == "mean") {
	                pval = gstpar.mean
	                t_skyval = t_skyval + pval
	            } else if (t_stat == "mode") {
	                pval = gstpar.mode
	                t_skyval = t_skyval + pval
	            } else {
	                pval = INDEF
	                t_skyval = 0.
	            }
	            imdelete (temp1, verify-, >& "dev$null")

	            # Write result into stdout and header.
	            if (t_verb)
	                printf ("Corrected sky for group %d:  %g\n", 
                                group, t_skyval)
	            if (t_skyname != "") {
	                    hedit (image // "[" // str(group) // "]", 
                                  t_skyname, t_skyval, add-, delete-, 
                                  verify-, show=t_verb, update+)
	            }
	        }
	    }
	}

	skyvalue = t_skyval

	# Recover called tasks' parameters.
	gsky.masks    = k_masks
	gsky.lower    = k_lower
	gsky.upper    = k_upper
	gsky.subsky   = k_subsk
	gsky.skyname  = k_skyname
	gsky.verbose  = k_verb
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
