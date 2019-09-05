# KOLMOV -- Kolmogorov-Smirnov test for goodness of fit.
#
# Original Fortran program, Tom Lougheed, Sept 86
# Rewritten, Chris Biemesderfer, May 88
# Corrected computation of neff, Bernie Simon, Apr 94

# S(n,x) is the distribution created from the sample and F(x) is the proposed
# distribution from which the sample is derived.  D being the discrepancy :
#
# (1) D+ = max[S(n,x) - F(x)]        is the upper one sided test.
#           x
#
# (2) D- = max[F(x) - S(n,x)]        is the lower one sided test.
#           x
#
# The test from Kolmogorov's original paper is:
#
# (3) D* = max|S(n,x) - F(x)|        i.e., the two sided test.
#           x
#
# The above formulas apply both to discrete and continuous distributions (since 
# discrete distributions are extended into continuous ones).  Continuous
# distributions are represented by discrete numbers as well.

procedure t_kolmov ()

pointer	infile		# Name of file to be tested
pointer reffile		# Name of reference file
pointer	ct		# Type of comparison

pointer sp, cd, in, ref

int	ndat		# Number of test data
int	nref		# Number of reference data
int	strdic()

real	dplus		# Maximum discrepancy
real	dminus		# Minimum discrepancy
real	dstar		# Absolute discrepancy
real	pplus		# Probability of lesser dplus than observed
real	pminus		# Probability of higher dminus than observed
real	pstar		# Probability of lesser dstar than observed
real	neff		# "Effective" number of sample points

begin
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (reffile, SZ_FNAME, TY_CHAR)
	call salloc (ct, SZ_LINE, TY_CHAR)

	# Get the file names.
	call clgstr ("infile", Memc[infile], SZ_FNAME)
	call clgstr ("reffile", Memc[reffile], SZ_FNAME)

	# Get the type of comparison desired by the user.  Acceptable values
	# of the parameter are :
	#    sample        -->  both files are data
	#    distribution  -->  data to be matched against a distribution
	#    uniform       -->  data are compared to a uniform distribution
	#    exponential   -->  data are compared to a exponential distribution
	#    powerlaw      -->  data are compared to a power law distribution

	call clgstr ("comptype", Memc[ct], SZ_LINE)

	# Load comparison type dictionary.
	call salloc (cd, SZ_LINE, TY_CHAR)
	call strcpy ("|distribution|sample|uniform", Memc[cd], SZ_LINE)
	call strcat ("|exponential|pareto|powerlaw", Memc[cd], SZ_LINE)

	# Read in sample data
	call ks_get_data (Memc[infile], in, ndat)

	# Perform sample/distribution comparison.
	switch (strdic (Memc[ct], Memc[ct], SZ_LINE, Memc[cd])) {

	case 1:		# "distribution"
	    call ks_get_data (Memc[reffile], ref, nref)
	    if (nref != ndat)
		call error (0, "sample and distribution must contain same number of points")
	    call asrt2r (Memr[in], Memr[in], Memr[ref], Memr[ref], ndat)
	    call ks_continuous (Memr[in], Memr[ref], ndat, dplus, dminus)
	    neff = ndat

	case 2:		# "sample"
	    call asrtr (Memr[in], Memr[in], ndat)
	    call ks_get_data (Memc[reffile], ref, nref)
	    call asrtr (Memr[ref], Memr[ref], nref)
	    call ks_discrete (Memr[in], ndat, Memr[ref], nref, dplus, dminus)
	    neff = real (ndat * nref) / real (ndat + nref)

	default:	# "uniform, exponential, pareto, powerlaw"
	    call asrtr (Memr[in], Memr[in], ndat)
	    call malloc (ref, ndat, TY_REAL)
	    call ks_distribution (Memc[ct], Memc[cd], Memr[in], Memr[ref], ndat)
	    call ks_continuous (Memr[in], Memr[ref], ndat, dplus, dminus)
	    call strcpy (Memc[ct], Memc[reffile], SZ_FNAME)
	    call strcat (" (generated)", Memc[reffile], SZ_FNAME)
	    nref = ndat
	    neff = ndat
	}

	# The envelope, please ...
	call printf ("# %s (%d pts) vs. %s (%d pts)\n")
	    call pargstr (Memc[infile])
	    call pargi (ndat)
	    call pargstr (Memc[reffile])
	    call pargi (nref)

	# Compute significance and report results.
	call ks_oneside (dplus, neff, pplus)

	call printf ("# Maximum discrepancy          Significance (percent)\n")
	call printf ("     %f                       %f\n")
	    call pargr (dplus)
	    call pargr (pplus*100)

	call ks_oneside (dminus, neff, pminus)

	call printf ("# Minimum discrepancy\n")
	call printf ("     %f                       %f\n")
	    call pargr (dminus)
	    call pargr (pminus*100)

	dstar = max (abs (dplus), abs (dminus))
	call ks_twoside (dstar, neff, pstar)

	call printf ("# Max abs discrepancy\n")
	call printf ("     %f                       %f\n")
	    call pargr (dstar)
	    call pargr (pstar*100)

	call mfree (ref, TY_REAL)
	call mfree (in, TY_REAL)
	call sfree (sp)
end
