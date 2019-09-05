include <tbset.h>

define	MAX_TESTS	5	# number of possible two-sample tests

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	04-Sep-92	revised to use asurv 1.2

# T_TWOST -- Driver routine for two sample tests

# This task computes several nonparametric two-sample tests for 
# comparing two or more censored data sets, giving a variety 
# of ways to test whether two censored samples are drawn from the 
# same parent population.  They are mostly generalizations of 
# standard tests for uncensored data, such as the Wilcoxon and 
# logrank nonparametric two-sample tests.  They differ in how the 
# censored data are weighted or "scored" in calculating the 
# statistic.  Thus each is more sensitive to differences at one end 
# or the other of the distribution.  The Gehan and logrank tests 
# are widely used in biometrics, while some of the others are not.
# 
# The two-sample tests are somewhat less restrictive than the 
# Kaplan-Meier estimator, since they seek only to compare two 
# samples rather than determine the true underlying distribution.  
# The Gehan test assume that the censoring patterns of the two 
# samples are the same, but does not require that they be random.  
# The logrank test results appear to be correct even when the 
# censoring patterns differ somewhat.  There is little known about 
# the limitations of the other two-sample tests given here.

procedure t_twost ()

#--
pointer	inlist		# input filename list descriptor
pointer	tests		# list of tests to be done
int	ifirst		# first group to use
int	isecond		# second group to use
bool	kaplan		# perform Kaplan Meier test?
bool	verbose		# print everything?

bool	start
int	ntot, list[MAX_TESTS]
pointer	sp1, filespec, filename, section
pointer	sp2, ind, ista, x, tp

int	ncol
data	ncol	/ 1 /

string	keywords "|gehan-permute|gehan-hyper|logrank|peto-peto|peto-prentice"

bool	clgetb()
int	clgeti(), imtgetim(), tbpsta(), tsdata()
pointer	imtopenp(), tbtopn()

begin
	# Allocate dynamic memory for strings

	call smark (sp1)
	call salloc (tests, SZ_FNAME, TY_CHAR)
	call salloc (filespec, SZ_FNAME, TY_CHAR)
	call salloc (filename, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_LINE, TY_CHAR)

	# Read task parameters

	inlist = imtopenp ("input")
	call clgstr ("tests", Memc[tests], SZ_FNAME)

	ifirst = clgeti ("first")
	isecond = clgeti ("second")
	kaplan = clgetb ("kaplan")
	verbose = clgetb ("verbose")

	call checklist (keywords, Memc[tests], list, MAX_TESTS)

	# Perform the two-sample tests

	start = true
	while (imtgetim (inlist, Memc[filespec], SZ_FNAME) != EOF) {

	    # Separate the filespec into filename and section fields

	    call asparse (Memc[filespec], Memc[filename], SZ_FNAME, 
			  Memc[section], SZ_LINE)

	    # Open the file as a table and read number of rows

	    tp = tbtopn (Memc[filename], READ_ONLY, 0)
	    ntot = tbpsta (tp, TBL_NROWS)

	    # Allocate memory to hold data

	    call smark (sp2)
	    call salloc (x, ntot, TY_DOUBLE)
	    call salloc (ind, ntot, TY_INT)
	    call salloc (ista, ntot, TY_INT)

	    # Read data from table

	    if (tsdata (tp, Memc[section], Memi[ind], 
			Memi[ista], Memd[x], ntot) == OK) {

		# Write title lines to output
		call tstitle (tp, Memc[section], ifirst, isecond, start) 

		# Calculate and print the results
		call twost (verbose, list, Memi[ind], Memi[ista], Memd[x], 
			    ifirst, isecond, ncol, ncol, ntot)

		# Calculate Kaplan-Meier estimator, if requested
		if (kaplan)
		    call ts_kmest (verbose, Memi[ind], Memi[ista], Memd[x], 
				   ifirst, isecond, ncol, ncol, ntot)
	    }

	    call tbtclo (tp)
	    call sfree (sp2)
	}

	# Close list of files

	call imtclose (inlist)
	call sfree (sp1)
end
