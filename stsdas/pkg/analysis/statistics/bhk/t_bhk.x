include <tbset.h>

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	18-Nov-92	revised to use asurv 1.2

# T_BHK -- Driver routine for generalized Kendall's tau (BHK method)

# This procedure computes the generalized Kendall's tau correlation 
# coefficient between two variables.  It can treat mixed censoring 
# including censoring in the independent variable, but can have only
# one independent variable.  It is known to be inaccurate when
# many tied values are present in the data.  Ties are particularly
# common when the data are binned.  Caution should be used in
# these cases.

procedure t_bhk ()

#--
pointer	inlist		# input filename list descriptor
bool	verbose		# print everything?

bool	first
double	zvalue, prob, tau
int	icens, nyc, nxc, nbc, ntot, nd, nc[8]
pointer	sp1, filespec, filename, section
pointer	sp2, ind, iaa, ibb, x, y, tp

bool	clgetb()
int	imtgetim(), tbpsta(), bhkdata()
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
	    call salloc (iaa, ntot, TY_INT)
	    call salloc (ibb, ntot, TY_INT)

	    # Read data from table

	    if (bhkdata (tp, Memc[section], Memi[ind], 
			 Memd[x], Memd[y], ntot) == OK) {

		# Write title lines to output

		call bhktitle (tp, Memc[section], first) 

		# Calculate and print the results

		call censor_statistics (verbose, Memi[ind], ntot, nd, nc, 
					icens, nyc, nxc, nbc)

		call bhk (Memi[ind], Memd[x], Memd[y], ntot, zvalue, prob, tau,
			  Memi[iaa], Memi[ibb])

		call bhkprint (zvalue, prob, tau)
	    }

	    call tbtclo (tp)
	    call sfree (sp2)
	}

	# Close list of files

	call imtclose (inlist)
	call sfree (sp1)
end
