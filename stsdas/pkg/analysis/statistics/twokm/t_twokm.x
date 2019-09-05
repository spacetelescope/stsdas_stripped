include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	11-Sep-92	revised to use asurv 1.2

# T_TWOKM -- Driver routine for Schmitt's linear regression bivariate test 

# "schmittbin" calculates the binned two-dimensional Kaplan-Meier distribution 
# and associated linear regression coefficients derived by Schmitt (1985).  
# Schmitt's binned linear regression can treat mixed censoring including 
# censoring in the independent variable, but can have only one 
# independent variable.  TWOKM requires only that the censoring distribution 
# about the fitted line is random.  Use of TWOKM is questionable in certain 
# cases with heavy censoring.  The user must arbitrarily choose a bin size.  
# If it is too small, many censored points at the end of the distribution 
# will be changed to detected points.  If the bins are too large, accuracy 
# in the regression calculation is reduced.  

procedure t_twokm ()

#--
pointer	inlist		# input filename list descriptor
int	nxbin		# Number of bins in independent variable
int	nybin		# Number of bins in dependent variable
double	xsize		# Size of bins in independent variable
double	ysize		# Size of bins in dependent variable
double	xorg		# Origin of bins in independent variable
double	yorg		# Origin of bins in dependent variable
double	tol		# Tolerance for regression fit
int	niter		# Maximum number of iterations for fit
int	nboot		# Number of bootstrap runs to estimate regression error
bool	verbose		# print everything?

bool	first, setbin
int	icens, nyc, nxc, nbc, ntot, nd, nc[8]
pointer	sp1, filespec, filename, section
pointer	sp2, ind, x, y, tp

bool	clgetb()
double	clgetd()
int	clgeti(), imtgetim(), tbpsta(), tkmdata()
pointer	imtopenp(), tbtopn()

begin
	# Allocate dynamic memory for strings

	call smark (sp1)
	call salloc (filespec, SZ_FNAME, TY_CHAR)
	call salloc (filename, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_LINE, TY_CHAR)

	# Read task parameters

	inlist = imtopenp ("input")

	nxbin = clgeti("nxbins")
	nybin = clgeti("nybins")
	xsize = clgetd("xsize")
	ysize = clgetd("ysize")
	xorg = clgetd("xorigin")
	yorg = clgetd("yorigin")
	setbin = xsize == 0.0 || ysize == 0.0

	tol = clgetd("tolerance")
	niter = clgeti("niter")
	nboot = clgeti("nboot")
	verbose = clgetb("verbose")

	# Compute Schmitt's linear regression

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
	    call salloc (y, ntot, TY_DOUBLE)
	    call salloc (ind, ntot, TY_INT)

	    # Read data from table

	    if (tkmdata (tp, Memc[section], Memi[ind], 
			 Memd[x], Memd[y], ntot) == OK) {

		# Write title lines to output

		call tkmtitle (tp, Memc[section], first) 

		# Calculate bin sizes if not set by user

		if (setbin)
		    call makebin (Memd[x], Memd[y], ntot, nxbin, nybin, 
				  xsize, ysize, xorg, yorg)

		# Calculate and print the results

		call censor_statistics (verbose, Memi[ind], ntot, nd, nc, 
					icens, nyc, nxc, nbc)

		call twokm (Memi[ind], Memd[x], Memd[y], ntot, nxbin, nybin, 
			    xsize, ysize, xorg, yorg, tol, niter, nboot, 
			    verbose, nc)
	    }

	    call tbtclo (tp)
	    call sfree (sp2)
	}

	# Close list of files

	call imtclose (inlist)
	call sfree (sp1)
end
