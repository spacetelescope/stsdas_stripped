include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	22-Sep-92	revised to use asurv 1.2

# T_EM -- Driver routine for EM method multivariate regression

# "emmethod" calculates linear regression coefficients assuming 
# a normal distribution of residuals.  The EM algorithm can treat 
# several independent variables if the dependent variable contains 
# only one kind of censoring (i.e. upper or lower limits).  
# EM requires that the residuals about the fitted line follow 
# a Gaussian distribution.  There is considerable uncertainty 
# regarding the error analysis of the regression coefficients of 
# the EM method.  

procedure t_em ()

#--
pointer	inlist		# input filename list descriptor
double	tol		# tolerance for regression fit
int	niter		# maximum number of iterations for fit
pointer	alpha		# initial estimates of regression coefficients
bool	emestim		# user provides initial fit estimate?
bool	verbose		# print everything?

bool	first
int	status, icens, nyc, nxc, nbc
int	ntot, nvar, nd, nc[8]
pointer	sp1, filespec, filename, section
pointer	sp2, ind, x, y, y2, tp

string	badcensor  "Censoring is allowed only in dependent variable"

bool	clgetb()
double	clgetd()
int	clgeti(), imtgetim(), tbpsta(), emdata(), word_count()
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
	emestim =  clgetb("emestim")
	verbose = clgetb("verbose")

	# Compute EM method linear regression

	first = true
	while (imtgetim (inlist, Memc[filespec], SZ_FNAME) != EOF) {

	    # Separate the filespec into filename and section fields

	    call asparse (Memc[filespec], Memc[filename], SZ_FNAME, 
			  Memc[section], SZ_LINE)

	    # Open the file as a table and read number of rows

	    tp = tbtopn (Memc[filename], READ_ONLY, 0)
	    ntot = tbpsta (tp, TBL_NROWS)

	    # Calculate nvar from section field

	    nvar = word_count (Memc[section]) - 1
	    nvar = max (1, nvar)

	    # Allocate memory to hold data

	    call smark (sp2)
	    call salloc (alpha, nvar+1, TY_DOUBLE)
	    call salloc (x, nvar*ntot, TY_DOUBLE)
	    call salloc (y, ntot, TY_DOUBLE)
	    call salloc (y2, ntot, TY_DOUBLE)
	    call salloc (ind, ntot, TY_INT)

	    # Read data from table

	    if (emdata (tp, Memc[section], Memi[ind], 
			Memd[x], Memd[y], Memd[y2], nvar, ntot) == OK) {

		# Write title lines to output

		call emtitle (tp, Memc[section], nvar, first) 

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

		# Read initial estimates of regression coefficients

		call emalpha (emestim, Memd[alpha], nvar)

		# Perform regression and print results

		call em (Memi[ind], Memd[x], Memd[y], Memd[y2], nvar, ntot, 
			 nyc, tol, niter, Memd[alpha])
	    }

	    call tbtclo (tp)
	    call sfree (sp2)
	}

	# Close list of files

	call imtclose (inlist)
	call sfree (sp1)
end
