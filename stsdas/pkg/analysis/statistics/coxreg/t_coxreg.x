include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	19-Nov-92	revised to use asurv 1.2

# T_COXREG -- Driver routine for Cox-Hazard multivariate test

# This procedure computes a correlation probability by Cox's 
# proportional hazard model.  The Cox hazard model can treat several 
# independent variables if the dependent variable contains only one 
# kind of censoring (i.e. upper or lower limits). The Cox correlation 
# is known to be inaccurate when many tied values are present in the 
# data. Ties are particularly common when the data is binned. Caution 
# should be used in these cases. Cox regression, though widely used 
# in biostatistical applications, formally applies only if the 
# "hazard rate" is constant; that is, the cumulative distribution 
# function of the censored variable falls exponentially with increasing 
# values. Astronomical luminosity functions, in contrast, are frequently 
# modeled by power law distributions.  It is not clear whether or 
# not the results of a Cox regression are significantly affected by 
# this difference.

procedure t_coxreg ()

#--
pointer	inlist		# input filename list descriptor
bool	verbose		# print everything?

bool	first
double	prob, chi
int	status, icens, nyc, nxc, nbc
int	ntot, nvar, nd, nc[8]
pointer	sp1, filespec, filename, section
pointer	sp2, ind, x, y, score, rinfo, finfo, il, im, tp

string	badcensor  "Censoring is allowed only in dependent variable"

bool	clgetb()
int	imtgetim(), tbpsta(), cxdata(), word_count()
pointer	imtopenp(), tbtopn()

begin
	# Allocate dynamic memory for strings

	call smark (sp1)
	call salloc (filespec, SZ_FNAME, TY_CHAR)
	call salloc (filename, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_LINE, TY_CHAR)

	# Read task parameters

	inlist = imtopenp ("input")
	verbose = clgetb("verbose")

	# Compute the Cox regression

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
	    call salloc (x, nvar*ntot, TY_DOUBLE)
	    call salloc (y, ntot, TY_DOUBLE)
	    call salloc (ind, ntot, TY_INT)
	    call salloc (score, nvar, TY_DOUBLE)
	    call salloc (rinfo, nvar*nvar, TY_DOUBLE)
	    call salloc (finfo, nvar*nvar, TY_DOUBLE)
	    call salloc (il, ntot, TY_INT)
	    call salloc (im, ntot, TY_INT)

	    # Read data from table

	    if (cxdata (tp, Memc[section], Memi[ind], 
			Memd[x], Memd[y], nvar, ntot) == OK) {

		# Write title lines to output

		call cxtitle (tp, Memc[section], nvar, first) 

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

		# Perform cox regression and print results

		call coxreg (Memi[ind], Memd[x], Memd[y], nvar, ntot, 
			     icens, chi, prob, Memd[rinfo], Memd[score],
			     Memd[finfo], Memi[il], Memi[im])

		call cxprint (nvar, prob, chi)
	    }

	    call tbtclo (tp)
	    call sfree (sp2)
	}

	# Close list of files

	call imtclose (inlist)
	call sfree (sp1)
end
