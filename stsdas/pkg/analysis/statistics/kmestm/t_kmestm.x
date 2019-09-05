include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	06-Aug-92	revised to use asurv 1.2

# T_KMESTM -- Driver routine for Kaplan-Meier estimator test

# "kmestimate" calculates and prints the Kaplan-Meier product-limit estimator 
# of a randomly censored distribution.  It is the unique, self-consistent, 
# generalized maximum-likelihood estimator for the population from 
# which the sample was drawn.  When formulated in cumulative form, 
# it has analytic asymptotic (for large N) error bars.  The median 
# is always well-defined, though the mean is not if the lowest 
# point in the sample is an upper limit.  
#
# The Kaplan-Meier estimator works with any underlying 
# distribution (e.g. Gaussian, power law, bimodal), but only if the 
# censoring is "random."  That is, the probability that the 
# measurement of an object is censored can not depend on the value 
# of the censored variable.  At first glance, this may seem to be 
# inapplicable to most astronomical problems:  we detect the 
# brighter objects in a sample, so the distribution of upper limits 
# always depends on brightness.  However, two factors often serve 
# to randomize the censoring distribution.  First, the censored 
# variable may not be correlated with the variable by which the 
# sample was initially identified.  Thus, infrared observations of 
# a sample of radio bright objects will be randomly censored if the 
# radio and infrared emission are unrelated to each other.  Second, 
# astronomical objects in a sample usually lie at different 
# distances, so that brighter objects are not always the most 
# luminous.  
#
# Thus, the censoring mechanisms of each study MUST be 
# understood individually to judge whether the censoring is or is 
# not likely to be random.  The appearance of the data, even if the 
# upper limits are clustered at one end of the distribution, is NOT 
# a reliable measure.  A frequent (if philosophically distasteful) 
# escape from the difficulty of determining the nature of the 
# censoring in a given experiment is to define the population of 
# interest to be the observed sample.  The Kaplan-Meier estimator 
# then always gives a valid redistribution of the upper limits, 
# though the result may not be applicable in wider contexts.

procedure t_kmestm ()

#--
pointer	inlist		# input filename list descriptor
double	binstart	# starting value of the first bin
double	binsize		# width of the bin
int	nbin		# number of bins
bool	diff		# compute differential estimator?
bool	verbose		# print everything?

bool	first
int	ntot
pointer	sp1, filespec, filename, section
pointer	sp2, ind, x, tp

int	ncol
data	ncol	/ 1 /

bool	clgetb()
double	clgetd()
int	clgeti(), imtgetim(), tbpsta(), kmdata()
pointer	imtopenp(), tbtopn()

begin
	# Allocate dynamic memory for strings

	call smark (sp1)
	call salloc (filespec, SZ_FNAME, TY_CHAR)
	call salloc (filename, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_LINE, TY_CHAR)

	# Read task parameters

	inlist = imtopenp ("input")

	binstart = clgetd ("binstart")
	binsize = clgetd ("binsize")
	nbin = clgeti ("nbin")

	diff = clgetb("diff")
	verbose = clgetb("verbose")

	# Compute the KM estimator

	first = true
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

	    # Read data from table

	    if (kmdata (tp, Memc[section], Memi[ind], Memd[x], 
			ntot) == OK) {

		# Write title lines to output
		call kmtitle (tp, Memc[section], first) 

		# Calculate and print the results
		call kmestm (Memi[ind], Memd[x], ncol, ncol, ntot, 
			     binstart, binsize, nbin, diff, verbose)
	    }

	    call tbtclo (tp)
	    call sfree (sp2)
	}

	# Close list of files

	call imtclose (inlist)
	call sfree (sp1)
end
