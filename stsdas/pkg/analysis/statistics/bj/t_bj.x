include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	10-Nov-92	revised to use asurv 1.2

# T_BJ -- Driver routine for Buckley James method multivariate regression

# The Buckley-James method of linear regression is similar to the EM method.
# It uses the Kaplan-Meier distribution of the data points instead of 
# assuming a normal distribution, as the EM method does.

procedure t_bj ()

#--
pointer	inlist		# input filename list descriptor
double	tol		# tolerance for regression fit
int	niter		# maximum number of iterations for fit
bool	verbose		# print everything?

bool	first
int	status, icens, nyc, nxc, nbc
int	ntot, nvar, nd, nc[8]
pointer	sp1, filespec, filename, section
pointer	sp2, alpha, ind, x, y, tp

string	badcensor  "Censoring is allowed only in dependent variable"

bool	clgetb()
double	clgetd()
int	clgeti(), imtgetim(), tbpsta(), bjdata(), word_count()
pointer	imtopenp(), tbtopn()

begin
	# Allocate dynamic memory for strings

	call smark (sp1)
	call salloc (filespec, SZ_FNAME, TY_CHAR)
	call salloc (filename, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_LINE, TY_CHAR)

	# Read task parameters

	inlist = imtopenp ("input")

	tol = clgetd("tolerance")
	niter = clgeti("niter")
	verbose = clgetb("verbose")

	# Compute BJ method linear regression

	first = true
	while (imtgetim (inlist, Memc[filespec], SZ_FNAME) != EOF) {

	    # Separate the filespec into filename and section fields

	    call asparse (Memc[filespec], Memc[filename], SZ_FNAME, 
			  Memc[section], SZ_LINE)

	    # Open the file as a table and read number of rows

	    tp = tbtopn (Memc[filename], READ_ONLY, 0)
	    ntot = tbpsta (tp, TBL_NROWS)

	    # Calculate nvar from section field

	    nvar = word_count (Memc[section]) - 2
	    nvar = max (1, nvar)

	    # Allocate memory to hold data

	    call smark (sp2)
	    call salloc (alpha, nvar+2, TY_DOUBLE)
	    call salloc (x, nvar*ntot, TY_DOUBLE)
	    call salloc (y, ntot, TY_DOUBLE)
	    call salloc (ind, ntot, TY_INT)

	    # Read data from table

	    if (bjdata (tp, Memc[section], Memi[ind], 
			Memd[x], Memd[y], nvar, ntot) == OK) {

		# Write title lines to output

		call bjtitle (tp, Memc[section], nvar, first) 

		# Count number of censored observations

		call censor_statistics (verbose, Memi[ind], ntot, nd, nc, 
					icens, nyc, nxc, nbc)

		# Check to see if independent variables are censored
		# Print error message and skip this file if so

		if (nxc != 0 || nbc != 0) {
		    status = OK
		    call errdata (badcensor, Memc[section], tp, status)
		    next
		}

		# Perform regression and print results

		call bj (Memi[ind], Memd[x], Memd[y], nvar, ntot, 
			 nd, nyc, icens, tol, niter, Memd[alpha])
	    }

	    call tbtclo (tp)
	    call sfree (sp2)
	}

	# Close list of files

	call imtclose (inlist)
	call sfree (sp1)
end
