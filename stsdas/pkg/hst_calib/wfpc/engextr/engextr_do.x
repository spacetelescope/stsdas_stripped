# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"
include	<imhdr.h>
define	SZ_INSTRUM	5

#  engextr_do -- Do the engineering data extraction and video averaging.
#		 Input files must have .x0h extension (_x0f.fits for FITS files)
#		 Mask files must have .x1h extension (_x1f.fits for FITS files)
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#
#  Date		Author		Description
#  ----		------		-----------
#  24-May-1991  J.-C. Hsu	coding
#  08-Dec-1993  J.-C. Hsu	modify for WFPC2
#  19-Mar-2003  J.-C. Hsu	fix a bug caused by iki calls (causes imask = 
#				ifile) first run result was wrong
#				Also allows FITS input.
#------------------------------------------------------------------------------
procedure engextr_do (fin, outflag, fout, outdir, errval, loc, verbose)

# Input
pointer	fin, fout	 			# file template pointer
int	outflag
char	outdir[SZ_PATHNAME]
int	errval
int	loc[NCMD]
bool	verbose

# Local
int	mask
char	ifile[SZ_FNAME], ofile[SZ_FNAME], imask[SZ_FNAME]
pointer	ipin, ipinmask
pointer	sp
pointer	tp
pointer	arrdata, arrmask
pointer	data, msb, lsb
pointer	colidn[NCOL]
char	mask_sig[SZ_FNAME]			# mask file name's "signature"
char	inroot[SZ_FNAME], outroot[SZ_FNAME]
char	inext[SZ_FNAME]
char	instrum[SZ_INSTRUM]
int	dummy, i, j
char	indir[SZ_FNAME]
int	npix, nrows, nchar
int	len_dir, len_root, len_ext

pointer	gf_map() 
pointer	imgs2s()
int	imtgetim()
int	imaccess()
int	fnldir(), fnroot(), fnextn()
int	andi()
bool	streq()
#==============================================================================
begin
	    # read the next file name in the template list
	    nchar = imtgetim (fin, ifile, SZ_FNAME)

	    if (verbose) {
	        call printf("Start processing file %s\n")
	            call pargstr(ifile)
	    }

	    len_dir  = fnldir (ifile, indir, SZ_FNAME)
	    len_root = fnroot (ifile, inroot, SZ_FNAME)
    	    len_ext  = fnextn (ifile, inext, SZ_FNAME)
	    if (outflag == DIR) {

		# if the output is a directory name, use the root name of the
		# input file and attach the extension of .tab
		call strcpy (inroot, outroot, SZ_FNAME)

		# Construct the output file name.
		call strcpy (outdir, ofile, SZ_FNAME)
		call strcat (outroot, ofile, SZ_FNAME)
		call strcat (".tab", ofile, SZ_FNAME)

	    } else if (outflag == TEMPL) 
	   	nchar = imtgetim (fout, ofile, SZ_FNAME)

	    # assume the mask file has the same name as the input file 
	    # except the extension is .x1h instead of .x0h
	    # (_x0f.fits instead of _x0f.fits for a FITS input file)
	    call strcpy (indir, imask, SZ_FNAME)
	
	    # GEIS case
	    if (len_ext == 3 && inext[3] == 'h') {
	    	call strcat (inroot, imask, SZ_FNAME)
	    	call strcpy (inext, mask_sig, SZ_FNAME)
	    	mask_sig[2] = '1'
	    	call strcat (".", imask, SZ_FNAME)
	    	call strcat (mask_sig, imask, SZ_FNAME)

	    # FITS case
	    } else {
	    	call strcpy (inroot, mask_sig, SZ_FNAME)
	    	mask_sig[len_root-1] = '1'
	    	call strcat (mask_sig, imask, SZ_FNAME)
	    	call strcat (".", imask, SZ_FNAME)
	    	call strcat (inext, imask, SZ_FNAME)
	    }

	    mask = imaccess(imask, READ_ONLY)

	    if (verbose) {
		if (mask == YES) {
		    call printf("Use mask file `%s`.\n")
			call pargstr(imask)
		} else {
		    call printf("File `%s` not available, no mask is used.\n")
			call pargstr(imask)
		}
	    }
	    # open input file 
	    ipin = gf_map (ifile, READ_ONLY, 0)

	    # which instrument?
	    call imgstr (ipin, "INSTRUME", instrum, SZ_INSTRUM)

	    # read data from input file
	    if (IM_NDIM(ipin) != 2)
		call error (1, "input file has incorrect dimension (!= 2)")
	    else {
		npix = IM_LEN(ipin, 2)
	        arrdata = imgs2s (ipin, ENGCOL, ENGCOL, 1, npix)
	    }

	    # allocate memory for mask
	    call smark (sp)
	    call salloc (arrmask, npix, TY_SHORT)

	    # open input mask, if there is any, and check its size
	    if (mask == YES) {
	        ipinmask = gf_map (imask, READ_ONLY, 0)
		if (IM_NDIM(ipinmask) != IM_NDIM(ipin)) {
		    call eprintf (
		       	"input mask and file no. %d differ in dimension\n")
			call pargi (i)
		    call error (1, "")
		}
		do j = 1, IM_NDIM(ipinmask) {
		    if (IM_LEN(ipin, j) != IM_LEN(ipinmask, j)) {
		    	call eprintf ( 
				"input mask and file no. %d differ in size\n")
			    call pargi (i)
			call error (1, "")
		    }
		}
	        arrmask = imgs2s (ipinmask, ENGCOL, ENGCOL, 1, npix)
	    } else 
		call amovks (short(OKVAL), Mems[arrmask], npix)

	    call salloc (data, npix, TY_INT)
	    call salloc (msb, npix, TY_INT)
	    call salloc (lsb, npix, TY_INT)

	    # preserve the positive values of input data, and separate the 
	    # two bytes into MSB and LSB
	    do i = 1, npix {
	    	dummy = Mems[arrdata+i-1]
	        Memi[data-1+i] = andi(dummy, 0FFFFx)
		Memi[lsb-1+i] = andi(dummy, 0FFx)
		Memi[msb-1+i] = andi(dummy,0FF00x) / 100x
	    }

	    # 94th to 102nd words are byte-swapped
	    do i = 94, 102, 2 {
	    	dummy = Mems[arrdata+i-1]
		Memi[msb-1+i] = andi(dummy, 0FFx)
		Memi[lsb-1+i] = andi(dummy,0FF00x) / 100x
		Memi[data-1+i] = Memi[msb-1+i] * 100x + Memi[lsb-1+i]
	    }

	    # open the output table
	    call engextr_out (ofile, ifile, tp, colidn)
	    nrows = 0

	    # do the video bias averaging
	    call engextr_gr (ifile, ipin, npix, tp, colidn, nrows)	    

	    # do the engineering data extraction
	    if (streq(instrum, "WFPC")) {
	    	call w_v3f (Memi[data], Mems[arrmask],
				tp, colidn, errval, nrows) 
	    	call w_v3g (Memi[data], Memi[msb], Memi[lsb], Mems[arrmask],
				tp, colidn, errval, nrows, loc) 
	    	call w_v3h (Memi[data], Memi[msb], Memi[lsb], Mems[arrmask],
				tp, colidn, errval, nrows) 
	    	call w_v3i (Memi[data], Memi[msb], Memi[lsb], Mems[arrmask],
				tp, colidn, errval, nrows) 
	    } else if (streq(instrum, "WFPC2")) {
	    	call u_vi3j (Memi[data], Mems[arrmask],
				tp, colidn, errval, nrows) 
	    	call u_vi3k (Memi[data], Memi[msb], Memi[lsb], Mems[arrmask],
				tp, colidn, errval, nrows, loc) 
	    	call u_vi3l (Memi[data], Memi[msb], Memi[lsb], Mems[arrmask],
				tp, colidn, errval, nrows) 
	    	call u_vi3m (Memi[data], Memi[msb], Memi[lsb], Mems[arrmask],
				tp, colidn, errval, nrows) 
	    } else
		call error (1, "illegal instrument")

	    # close input file(s)
	    call gf_unmap (ipin)
	    if (mask == YES)
		call gf_unmap (ipinmask)

	    # close output table
	    call tbtclo (tp)
	    if (verbose) {
		call printf ("Output table %s created\n")
		    call pargstr (ofile)
	    }

	    call sfree (sp)
end
