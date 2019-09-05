include <tbset.h>

# retrieve quality comments from PDQ file (text file)

procedure pp_qual (rootname, qual, qualcom, ncom)

char	rootname[ARB]		# (PDQ) file root name
char	qual[SZ_LINE]
char	qualcom[SZ_LINE, ARB]
int	ncom			# number of quality comments

char	fname[SZ_LINE]
int	fd
char	buf[SZ_LINE]
char	dummy[8]

int	open()
int	access()
int	getline()
bool	streq()

begin
	ncom = 0

	call sprintf (fname, SZ_LINE, "%s.pdq")
	    call pargstr (rootname)

	if (access (fname, 0, 0) == NO) {
	    qual[1] = EOS
	    ncom = 1
	    call sprintf (qualcom[1,1], SZ_LINE,"(Cannot access the PDQ file.)")
	    return
	}

	fd = open (fname, READ_ONLY, TEXT_FILE)

	# read each card
	while (getline (fd, buf) != EOF) {
	    call strcpy (buf, dummy, 8)
	    if (streq (dummy, " QUALITY")) {
		call qual_val (buf, qual)
		next
	    } else if (streq (dummy, " QUALCOM")) {
		ncom = ncom + 1
		call qual_val (buf, qualcom[1,ncom])
		next
	    }
	}
	call close (fd)
end

# retrieve quality comments from PDQ file (FITS table)

procedure pp_qualfits (fname, qual, qualcom, ncom)

char	fname[ARB]		# (PDQ) file name
char	qual[SZ_LINE]
char	qualcom[SZ_LINE, ARB]
int	ncom			# number of quality comments

pointer	pdq			# PDQ file pointer
pointer colptr
int     nrows, i
char	buf[SZ_LINE]
char	dummy[10]

int	access()
int	strmatch()
pointer tbtopn()
int     tbpsta()

begin
	ncom = 0

	# open the PDQ table
	if (access (fname, 0, 0) == NO) {
            pdq = NULL
	    qual[1] = EOS
            call strcpy ("(Cannot access the PDQ file.)", qualcom[1,1],
                         SZ_LINE)
	    ncom = 1
	    return
	}

        pdq = tbtopn (fname, READ_ONLY, 0)

        # find the (only) column in the trailer file
        call tbcfnd1 (pdq, "TEXT_FILE", colptr)
	if (colptr == NULL) {
	    call tbcfnd1 (pdq, "PDQ-FILE", colptr)
	    if (colptr == NULL) {
	    	call printf ("PDQ Text Column not found in %s\n")
			call pargstr (fname)
            	call tbtclo (pdq)
	    	return
	    }
	}
 
        # find how many rows are there
        nrows = tbpsta (pdq, TBL_NROWS)
 
        # go through each row
        do i = 1, nrows {
            call tbegtt (pdq, colptr, i, buf, SZ_LINE)
 
            # look for the string QUALITY or QUALCOM
	    call strcpy (buf, dummy, 9)
	    if (strmatch (dummy, "QUALITY") != 0) {
		call qual_val (buf, qual)
		next
	    } else if (strmatch (dummy, "QUALCOM") != 0) {
		ncom = ncom + 1
		call qual_val (buf, qualcom[1,ncom])
		next
	    }
	}

	# close the PDQ table
        if (pdq != NULL) call tbtclo (pdq)
end

procedure qual_val (str, out)

char	str[ARB]
char	out[ARB]

int	i, j

begin
	do i = 11, SZ_LINE
	    if (str[i] == '\'') break
	
	if (i == SZ_LINE) {
	    out[1] = EOS
	} else {
	    do j = i+1, SZ_LINE {
	   	if (str[j] != '\'' && str[j] != EOS)
		    out[j-i] = str[j]
		else {
		    out[j-i] = EOS
		    break
		}
	    }
	}
end
