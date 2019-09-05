# Read the association table and get the rootnames of the backgrounds,
# and each individual exposure.

include	<tbset.h>

procedure npp_assoc (asn, bkroot, nbk, exproot, nexp)

char	asn[SZ_FNAME]			# input association table name
char	bkroot[SZ_FNAME, ARB]		# background root names
int	nbk				# number of background exposures
char	exproot[SZ_FNAME, ARB]		# exposure root names
int	nexp				# number of individual exposures

pointer	tb			
pointer	memname, memtype
int	nrows, i
char	buf[SZ_FNAME]
char	buf2[SZ_FNAME]

pointer	tbtopn()
int   	strmatch()
int	tbpsta()

begin
	# reset the counter
	nbk = 0
	nexp = 0

	# open the association table
        iferr (tb = tbtopn (asn, READ_ONLY, 0)) {
	    call printf ("Cannot open the association table '%s'\n")
		call pargstr (asn)
	    return
	}
	
	# find the columns in the table
	call tbcfnd1 (tb, "MEMNAME", memname)
	call tbcfnd1 (tb, "MEMTYPE", memtype)

	# find how many rows are there
	nrows = tbpsta (tb, TBL_NROWS)

	# go through each row
	do i = 1, nrows {
	    call tbegtt (tb, memtype, i, buf, SZ_FNAME)
	
	    # look for the word "PROD-BCK"
            if (strmatch(buf, "PROD-BCK") != 0) {
		nbk = nbk + 1

		# get the root name (in upper case)
	        call tbegtt (tb, memname, i, buf2, SZ_FNAME)
		call strcpy (buf2, bkroot[1,nbk], SZ_FNAME)
	
	    # look for the word "EXP-" (both EXP-TARG and EXP-BCKi)
	    } else if (strmatch(buf, "EXP-") != 0) {
		nexp = nexp + 1

		# get the root name (in upper case)
	        call tbegtt (tb, memname, i, buf2, SZ_FNAME)
		call strcpy (buf2, exproot[1,nexp], SZ_FNAME)
	    }
	}

	# close the table
	if (tb != NULL) call tbtclo (tb)
end
