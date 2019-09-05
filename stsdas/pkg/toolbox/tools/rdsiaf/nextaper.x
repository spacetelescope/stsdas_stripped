include	<tbset.h>

define	LIST1VAL  "siap_id,sics_v2,sics_v3,shape,maj_axis,macro_flg,boa_flg,\
boa_thresh,macro_id"

#* HISTORY *
#* B.Simon	09-Mar-89	Original
#* B.Simon	19-Jun-92	Rewritten to produce an sdas table
#* B.Simon	12-Nov-97	Modified to remove cdbs dependence

# NEXTAPER -- Locate the next aperture in the SIAF and read its description

int procedure nextaper (fd, tp, pdbdate, shape, sias_flag, npoly)

int	fd		# i: The file descriptor of the SIAF file
pointer	tp		# i: The output table pointer
char	pdbdate[ARB]	# i: The date read from the SIAF file
char	shape[ARB]	# o: Aperture shape
bool	sias_flag	# o: Sias coordinate transformation flag
int	npoly		# o: Degree of SIAS tranformation polynomial
#--
char	list[80,4], fmt[40,4]
int	cp, status, nrow, ncol, icol, irec, ilist, ifmt, clen, junk, ic
pointer	sp, cname, cfmt, cval, record, errmsg, field

equivalence (list[1,1],list1)
equivalence (list[1,2],list2)
equivalence (list[1,3],list3)
equivalence (list[1,4],list4)

equivalence (fmt[1,1],fmt1)
equivalence (fmt[1,2],fmt2)
equivalence (fmt[1,3],fmt3)
equivalence (fmt[1,4],fmt4)

string	notfound "Expected AJ record not found: %s"
string	nocolumn "nextaper: table column not found (%s)"

string	list1    LIST1VAL
string	list2    "min_axis,plate_scale,area,rot_angle,sias_flg"
string	list3    "parity,n_poly,sias_x,sias_y,sics_x,sics_y"
string	list4    ""

string	fmt1     "s10,f15,f15,s4,f15,s1,s1,f5,s4"
string	fmt2     "f15,f15,f15,f15,s1"
string	fmt3     "d2,d1,f15,f15,f15,f15"
string	fmt4     ""

string	savers   "shape,sias_flg,n_poly"

int	word_fetch(), word_match(), tbpsta(), tbcigi()
int	getline(), gstrcpy(), ctoi()
pointer	tbcnum()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (cname, SZ_FNAME, TY_CHAR)
	call salloc (cfmt, SZ_FNAME, TY_CHAR)
	call salloc (cval, SZ_LINE, TY_CHAR)
	call salloc (record, SZ_LINE, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Set up to search for the first aperture record

	status = EOF
	while (getline (fd, Memc[record]) > 72) {
	    if (Memc[record+71] == 'A' && Memc[record+72] == 'J') {
		status = OK
		break
	    }
	}
	if (status == EOF)
	    return (status)

	# Create a new table row and initialize its values to the defaults

	nrow = tbpsta (tp, TBL_NROWS) + 1
	ncol = tbpsta (tp, TBL_NCOLS) 

	do icol = 1, ncol {
	    cp = tbcnum (tp, icol)
	    if (tbcigi (cp, TBL_COL_DATATYPE) < 0) {
		call tbeptt (tp, cp, nrow, " ")
	    } else {
		call tbeptr (tp, cp, nrow, 0.0)
	    }
	}
	
	call tbcfnd (tp, "pdbdate", cp, 1)
	call tbeptt (tp, cp, nrow, pdbdate)

	# Read the next four records

	do irec = 1, 4 {

	    # Check to see if the record is there

	    if (irec > 1) {
		if (getline (fd, Memc[record]) == EOF) {
		    call sprintf (Memc[errmsg], SZ_LINE, notfound)
			call pargstr ("<End of file>")
		    call error (1, Memc[errmsg])	
		}

		if (Memc[record+71] != 'A' || Memc[record+72] != 'J') {
		    call siaf_trim (Memc[record])
		    call sprintf (Memc[errmsg], SZ_LINE, notfound)
			call pargstr (Memc[record+70])
		    call error (1, Memc[errmsg])	
		}
	    }

	    # Read the column values from the record into the database

	    ifmt = 1
	    ilist = 1
	    field = record

	    while (word_fetch (fmt[1,irec], ifmt, Memc[cfmt], SZ_FNAME) > 0) {

		# Extract the next field from the record

		ic = 1
		junk = ctoi (Memc[cfmt+1], ic, clen)
		field = field + gstrcpy (Memc[field], Memc[cval], clen)
		junk = word_fetch (list[1,irec], ilist, Memc[cname], SZ_FNAME)

		# Copy it to the output table

		call tbcfnd (tp, Memc[cname], cp, 1)
		if (cp == NULL) {
		    call sprintf (Memc[errmsg], SZ_LINE, nocolumn)
			call pargstr (Memc[cname])
		    call error (1, Memc[errmsg])	

		}

		call tbeptt (tp, cp, nrow, Memc[cval])
		
		# The value of certain fields controls the format
		# of the data file. Save these for later use
		
		switch (word_match (Memc[cname], savers)) {
		case 1:
		    call strcpy (Memc[cval], shape, clen)
		case 2:
		    sias_flag = Memc[cval] == 'Y'
		case 3:
		    ic = 1
		    junk = ctoi (Memc[cval], ic, npoly)
		}
	    }
	}

	call sfree (sp)
	return (status)
end
