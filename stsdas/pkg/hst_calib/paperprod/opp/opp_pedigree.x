# determine the pedigree information for STIS

include	<tbset.h>
include	"opp.h"

define	SZ_CORR		5

procedure opp_pedigree (trl, pedigree)

char	trl[SZ_FNAME]		# input (trailer) file descripter
char	pedigree[SZ_LINE, ARB]

pointer	tb			
pointer	colptr
int	nrows, i, j, n
int	indx, code
char	buf[SZ_LINE]
char	str[SZ_LINE]
char	corr[SZ_CORR]

pointer	tbtopn()
int   	strmatch()
int	strdic()
int	tbpsta()
int	strlen()

begin
	# initialize the output strings
	do i = 1, NREF {
	    pedigree[1,i] = EOS
	}

	# open the trailer file (a table)
        iferr (tb = tbtopn (trl, READ_ONLY, 0))
	    return
	
	# find the (only) column in the trailer file
	call tbcfnd1 (tb, "TEXT_FILE", colptr)

	# find how many rows are there
	nrows = tbpsta (tb, TBL_NROWS)

	# go through each row
	do i = 1, nrows {
	    call tbegtt (tb, colptr, i, buf, SZ_LINE)
	
	    # look for the word "PEDIGREE"
            if (strmatch(buf, "PEDIGREE") != 0) {
		
		# extract the pedigree value
		indx = strmatch (buf, "=")
		call strcpy (buf[indx], str, SZ_LINE)

		# get the calibration keyword from the line above
		if (i > 1) {
	    	    call tbegtt (tb, colptr, i-1, buf, SZ_LINE)
		    call strcpy (buf, corr, SZ_CORR)
            	    code = strdic (corr, corr, SZ_CORR, CALIB)
		}

		# read the next line to get descrip
		if (i < nrows) {
	    	    call tbegtt (tb, colptr, i+1, buf, SZ_LINE)
            	    if (strmatch(buf, "DESCRIP") != 0) {

			# fill in some blanks
			n = 10 - strlen (str)
			do j = 1, n
			    call strcat (" ", str, SZ_LINE)
		
		        # extract the pedigree value
		        indx = strmatch (buf, "=")
		        call strcat (buf[indx], str, SZ_LINE)
		    }
		}
		if (code != 0) {
		    call strcpy (str, pedigree[1, code], SZ_LINE)
		} else {
		    call printf ("No matching reference file keyword for %s\n")
			call pargstr (corr)
		}
	    }
	}

	# close the table
	if (tb != NULL) call tbtclo (tb)
end
