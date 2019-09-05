include	<imhdr.h>
include	"pplist.h"


# produce a list of unique ACS products from original input list
#   This list will contain rootname and file extension of product
#   in separate columns.
 
procedure t_jpp_prods()

pointer	tpin			    # template of input files
char    outlist[SZ_LINE]    # output list of unique products rootnames and
                            #   their extensions (RAW,CRJ,SFL,FLT)
char    outprods[SZ_LINE]   # list of full filenames for unique products

int	fd, fp			# output file pointer
char	fname[SZ_FNAME]		# input file name
char	root[SZ_FNAME]		# input file root name
pointer	raw             	# input file pointers
char	raw_ext[SZ_EXT]		# file extension name
char	flt_ext[SZ_EXT]		# file extension name
char	sfl_ext[SZ_EXT]		# file extension name
char	crj_ext[SZ_EXT]		# file extension name
char	out_ext[SZ_EXT]		# file extension name
char	outroot[SZ_FNAME]	# rootname of unique product

int nchar
char    dummy[SZ_FNAME]
int	n, nobs

int	open()
int	imtlen()
int	imtgetim()
pointer immap()
pointer imtopenp()

begin
    # Read in name of input list
    tpin = imtopenp("input")
    
    # Read in output list name
    call clgstr ("output", outlist, SZ_LINE)
    call clgstr ("outprods", outprods, SZ_LINE)

	# construct necessary file name extensions
	call strcpy ("_raw.fits", raw_ext, SZ_EXT) 
	call strcpy ("_flt.fits", flt_ext, SZ_EXT) 
	call strcpy ("_sfl.fits", sfl_ext, SZ_EXT) 
	call strcpy ("_crj.fits", crj_ext, SZ_EXT) 

    nobs = imtlen(tpin)

	# open the output file
	fp = open (outprods, APPEND, TEXT_FILE)
    fd = open (outlist, APPEND, TEXT_FILE)
    
    # loop all root names
    do n = 1, nobs {

        # read the next input image name in the template list
        nchar = imtgetim (tpin, root, SZ_FNAME)

	    # construct file names of the raw data, the trailer, and the jitter,
	    # open the 'raw' file which best represents the association
        # We start with RAW file and use it if it is NOT part of an ASN
        #   Next, we look for an SFL file
        #   Then, we would use the DTH file 
        #   Finally, we would use a CRJ file if it existed
        #   If none of these exist, then skip this rootname...
        # We just want to try and only use ** 1 ** observation per ASN
	    call strcpy (root, fname, SZ_FNAME)
	    call strcat (flt_ext, fname, SZ_FNAME)
        call strcat ("[0]", fname, SZ_FNAME)
	    call strcpy (flt_ext, out_ext, SZ_FNAME)
	    call strcpy (root, outroot, SZ_FNAME)
	    iferr (raw = immap (fname, READ_ONLY, 0)) {
	        call strcpy (root, fname, SZ_FNAME)
	        call strcat (sfl_ext, fname, SZ_FNAME)
            call strcat ("[0]", fname, SZ_FNAME)
	        call strcpy (sfl_ext, out_ext, SZ_FNAME)
	        call strcpy (root, outroot, SZ_FNAME)
	        iferr (raw = immap (fname, READ_ONLY, 0)) {
	            call strcpy (root, fname, SZ_FNAME)
	            call strcat (crj_ext, fname, SZ_FNAME)
                call strcat ("[0]", fname, SZ_FNAME)
	            call strcpy (crj_ext, out_ext, SZ_FNAME)
	            call strcpy (root, outroot, SZ_FNAME)
	            iferr (raw = immap (fname, READ_ONLY, 0)) {
	                call strcpy (root, fname, SZ_FNAME)
	                call strcat (raw_ext, fname, SZ_FNAME)
                    call strcat ("[0]", fname, SZ_FNAME)
	                iferr (raw = immap (fname, READ_ONLY, 0)) {
		                call printf ("%s: cannot open input raw file, skip.\n")
		                call pargstr (root)
		                next
		            } else {
                        # For this 'RAW' file, check to see if it is part of
                        # an association, if so, ignore in favor of using one
                        # of the products (SFL,DTH, or CRJ files) instead .
		                call imgstr (raw, "ASN_TAB", dummy, SZ_LINE)
		                if (dummy[1] != EOS) {
		                    call imunmap (raw)
		                    next
		                } else{
	                        call strcpy (raw_ext, out_ext, SZ_FNAME)
	                        call strcpy (root, outroot, SZ_FNAME)                        
                        }
                    } # Found a RAW file
                } # Found a CRJ file
            } # Found a SFL file
        } # Found a FLT file
        call imunmap (raw)
        call fprintf (fd, "%s  %s \n")
            call pargstr (outroot)
            call pargstr (out_ext)
        call fprintf (fp, "%s \n")
            call pargstr (fname)
    # End of loop over all root names (nobs)
    }

	# close output file
	call close (fd)
    call close (fp)
end
