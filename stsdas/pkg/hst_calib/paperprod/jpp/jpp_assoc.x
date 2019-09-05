# Read the association table and get the rootnames of the input exposures
# to the given product.
#
# This also returns the number of pattern positions, repeat-obs positions,
# and cr-split positions.  The list of rootnames returned will also be 
# ordered such that for each pattern position, the repeat-obs/cr-split
# positions will be in order.  The list can then be looped over each pattern
# position, then each repeat/split position for each patt-pos to access the
# input exposures in order.  
#
# The number of repeat-obs or cr-split positions will be zero for the one
# mode not being used (i.e., nrpt = 0, if cr-split is used).
# The number of pattern positions will be one if NOT a dither product.
#
include	<tbset.h>
include "jpp.h"

procedure jpp_assoc (root, prodext, explist, npatt, nrpt, nsplit)

char	root[ARB]			# input product filename
char	prodext[ARB]		    # input product extension
char	explist[SZ_FNAME, ARB]		# list of input exposure root names
int     npatt           # number of pattern positions used to create product
int     nrpt            # number of repeat-obs exposures/pattern position
int     nsplit          # number of cr-splits/pattern position

int	fd			# product file pointer
pointer	tb		# pointer for asn table	
int fexp        # pointer to input file
pointer	memname, memtype
int	nrows, i
int pattstep
char    pattern1[SZ_FNAME]
char    asn[SZ_FNAME]
int     nexp    # total number of input exposures

char	buf[SZ_FNAME]
char	buf2[SZ_FNAME]
char    prodname[SZ_FNAME]

pointer	immap()
pointer	tbtopn()
int   	strmatch()
int	tbpsta()
int    imgeti()
bool   streq()

begin
	# reset the counter
	nexp = 0

    # build name of product
    call strcpy (root, prodname, SZ_FNAME)
    call strcat (prodext, prodname, SZ_FNAME)
    call strcat ("[0]", prodname, SZ_FNAME)
    
    fd = immap (prodname, READ_ONLY, 0)
    
    # Read in pattern keywords, and set appropriate defaults
	iferr(call imgstr (fd, "PATTERN1", pattern1, SZ_FNAME))
        call strcpy("NONE",pattern1,SZ_FNAME)

	iferr(call imgstr (fd, "ASN_TAB", asn, SZ_FNAME))
        call strcpy("NONE",asn ,SZ_FNAME)

    # number of steps in pattern
    iferr(npatt = imgeti (fd, "P1_NPTS"))
        npatt = 0
    

    # which step in the pattern this product corresponds to
    iferr(pattstep = imgeti (fd, "PATTSTEP"))
        pattstep = 0

    # is this product a repeat-obs combined product...
    iferr(nrpt = imgeti (fd, "NRPTEXP"))
        nrpt = 0

    # or a cr-combined product?
    iferr(nsplit = imgeti (fd, "CRSPLIT"))
        nsplit = 0


    call imunmap(fd)
            

    # Safeguard here: if no ASN table specified in product header, return
    if (streq(asn, "NONE")) {
	    call printf ("No association table to open for this product.\n")
        return
    }        

	# open the association table
    # We need to go through it and find all the input exposures 
    #   (memtype = EXP*) that was combined to create the given product
    #    
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
    nexp = 0

	# go through each row
	do i = 1, nrows {
	    call tbegtt (tb, memtype, i, buf, SZ_FNAME)
	
        if (strmatch(buf, "EXP-") != 0) {
		    # get the root name (in upper case)
	        call tbegtt (tb, memname, i, buf, SZ_FNAME)
            
            # Now, test to see if this exposure belongs to the 
            # product we are processing...
            call strlwr(buf)
            call strcpy(buf, buf2, SZ_FNAME)
            call strcat("_raw.fits[0]",buf2, SZ_FNAME)
            iferr(fexp = immap(buf2, READ_ONLY, 0))
                next
            else {    
                # Figure out what we need to look for in the input exposures to
                # match them to the product...
                if (npatt == 0 || streq(prodext,"dth")) {
                    # This is the final dither-product or 
                    #   it is a simple repeat-obs/cr-split product... 
                    # All exposures belong to this product!
                    nexp = nexp + 1
		            call strcpy (buf, explist[1,nexp], SZ_FNAME)               
                } else {
                    # Find inputs for the same pattern-position as product
                    if (imgeti(fexp, "PATTSTEP") == pattstep) {
                        nexp = nexp + 1
		                call strcpy (buf, explist[1,nexp], SZ_FNAME)               
                    }                        
                }
            }
            # Close this input exposure
            call imunmap(fexp)
	    }
	}
        
	# close the table
	if (tb != NULL) call tbtclo (tb)
end
