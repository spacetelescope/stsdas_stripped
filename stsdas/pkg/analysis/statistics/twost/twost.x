define	DECPLACE	3

#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	04-Sep-92	revised to use asurv 1.2

# TWOST -- Perform non-parametric two sided tests of censored data

procedure twost (verbose, list, ind, ista, x, ifirst, isecond, 
		 jcol, ncol, ntot)

bool	verbose		# i: long form of printout		
int	list[ARB]	# i: list of tests to perform
int	ind[ARB]	# i: censor indicator
int	ista[ARB]	# i: group number
double	x[ncol,ARB]	# i: data values
int	ifirst		# i: first group number
int	isecond		# i: second group number
int	jcol		# i: data column to analyze
int	ncol		# i: number of data columns
int	ntot		# i: number of data rows
#--
int	i, iu, ic, n1, n2, ncomp, isign, ng
pointer	sp, xy, id1, id2, r, xm, z, e1, h

string	title1  "%4w # of Data Points :%6d # of %s Limits :%6d \n"
string	title2  "%4w # of Data Points in Group %d :%6d \n"
string	title3  "\n Distinct Failures%9wR(i)%12wM(i)%8wM(i)/R(i)%9wH(i)\n\n"
string	nogroup "No data points were found in first or second groups"
string	nopeto  "\n Peto & Prentice test reduces to Gehan test when there \
are no censored observations \n"

double	round()

begin
	# Allocate dynamic memory for work arrays

	call smark (sp)
	call salloc (xy, ntot, TY_DOUBLE)
	call salloc (id1, ntot, TY_INT)
	call salloc (id2, ntot, TY_INT)
	call salloc (r, ntot, TY_DOUBLE)
	call salloc (xm, ntot, TY_DOUBLE)
	call salloc (z, ntot, TY_DOUBLE)
	call salloc (e1, ntot, TY_DOUBLE)
	call salloc (h, ntot, TY_DOUBLE)
	

	# Copy relevant observations into work arrays
	# Count the number of observations from each group
	# and the number of censored observations

	call ts_sort (ind, ista, x, jcol, ncol, ntot)
	call aarray (x, ind, ista, jcol, ncol, ntot, ifirst, isecond, 
		     Memd[xy], Memi[id1], Memi[id2], iu, ic, n1, n2, 
		     ncomp, isign)

	if (n1 == 0 || n2 == 0)
	    call error (1, nogroup)

	call arisk (Memd[xy], Memi[id1], ncomp, ntot, Memd[r], Memd[xm],
		    Memd[z], Memd[e1], ng, Memd[h])

	# Print general information about observations

	call printf (title1)
	call pargi (ncomp)
	if (isign == 1) {
	    call pargstr ("Lower")
	} else {
	    call pargstr ("Upper")
	}
	call pargi (ic)

	do i = 1, 2 {
	    call printf (title2)
	    call pargi (i)
	    if (i == 1) {
		call pargi (n1)
	    } else {
		call pargi (n2)
	    }
	}

	# Print more detailed information about risk sets

	if (verbose) {
	    call printf (title3)

	    do i = 0, ng-1 {
		call printf ("  %14.3f %14.3f %14.3f %14.3f %14.3f \n")
		call pargd (isign * Memd[z+i])
		call pargd (round (Memd[r+i], DECPLACE))
		call pargd (round (Memd[xm+i], DECPLACE))
		call pargd (round (Memd[e1+i], DECPLACE))
		call pargd (round (Memd[h+i], DECPLACE))
	    }
	    call printf ("%61w %14.3f\n\n")
	    call pargd (round (Memd[h+ng], DECPLACE))
	}

	# Peform two sided tests chosen by user and print results

	if (list[1] == YES)
	    call do_gehan (verbose, isign, Memi[id1], Memi[id2], Memd[xy], 
			   ic, n1, n2, ncomp, ntot)

	if (list[2] == YES)
	    call do_wlcxn (verbose, isign, Memi[id1], Memi[id2], Memd[xy], 
			   n1, n2, ncomp, ntot)

	if (list[3] == YES)
	    call do_lrank (verbose, isign, Memi[id1], Memi[id2], Memd[xy], 
			   n1, n2, ncomp, ntot)

	if (list[4] == YES)
	    call do_pwlcxn (verbose, isign, Memi[id1], Memi[id2], Memd[xy], 
			    Memd[xm], Memd[h], ic, n1, n2, ncomp, ntot)

	if (list[5] == YES) {
	    if (ic == 0 && list[1] == YES) {
		call printf (nopeto)

	    } else {
		call do_peto (verbose, isign, Memi[id1], Memi[id2], Memd[xy], 
			      n1, n2, ncomp, ntot)
	    }
	}

	call sfree (sp)
end
