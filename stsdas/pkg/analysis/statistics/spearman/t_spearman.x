include <tbset.h>

#* HISTORY *
#* B.Simon	23-Nov-92	adapted from asurv 1.2

# T_SPEARMAN -- Driver routine for computation of Spearman's rho

# This program computes a correlation probability between two variables
# by Spearman's rho. For censored data points, Akritas' ranking method 
# is used.

procedure t_spearman ()

#--
pointer	inlist		# input filename list descriptor
bool	verbose		# print everything?

bool	first
double	prob, rho
int	status, icens, nyc, nxc, nbc, ntot, nd, nc[8]
pointer	sp1, filespec, filename, section
pointer	sp2, ind, x, y, indx, xf, xx, tp

string	nodetect  "No detections in dataset, file skipped."

bool	clgetb()
int	imtgetim(), tbpsta(), sp_data()
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

	    call salloc (xf, 2*ntot, TY_DOUBLE)
	    call salloc (xx, 2*ntot, TY_DOUBLE)
	    call salloc (indx, 2*ntot, TY_INT)

	    # Read data from table

	    if (sp_data (tp, Memc[section], Memi[ind], 
			 Memd[x], Memd[y], ntot) == OK) {

		# Write title lines to output

		call sp_title (tp, Memc[section], first) 

		call censor_statistics (verbose, Memi[ind], ntot, nd, nc, 
					icens, nyc, nxc, nbc)

		# Make sure there is at least one detection

		if (nd == 0) {
		    status = OK
		    call errdata (nodetect, Memc[section], tp, status)
		    next
		}

		# Calculate and print the results

		call sprman (Memi[ind], Memd[x], Memd[y], ntot, Memi[indx], 
			     Memd[xf], Memd[xx], rho, prob)

		call sp_print (ntot, rho, prob)
	    }

	    call tbtclo (tp)
	    call sfree (sp2)
	}

	# Close list of files

	call imtclose (inlist)
	call sfree (sp1)
end
