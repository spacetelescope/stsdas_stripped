include <imio.h>
include <imhdr.h>
include <fset.h>
include <error.h>

define	LEN_EXTN	3
# CPSUN_VAX -- Procedure to convert a SUN SDAS image into a VMS SDAS image.
# The input image should have UNIX floating point number (IEEE). It will
# convert any UNIX/IRAF datatype into a VMS/IRAF datatype; i.e. no consideration
# for small or big number is taken when converting.

procedure hhhvax_sun (image1, image2, verbose)

char	image1[SZ_FNAME], image2[SZ_FNAME]
bool	verbose

pointer im, buf, sp, pp
char	root[SZ_FNAME], extn[LEN_EXTN]
char	pname[SZ_PATHNAME]
int	v1[IM_MAXDIM]
int	fdh, pfd, istat, i, nl
int	gn, npix, junk, szgroup
int	impnls(), impnlr(), impnll(), impnld()
int	open(), immap(), strncmp(), read()
int	gi_gstfval()

bool    bterm
int	fstati(), clktime()

real	datamin, datamax
int	save, sz_pixfile

include "feed_user.com"

errchk  open, immap
begin

	# Open file on a remote machine. 

	iferr (fdh = open (image1, READ_ONLY, TEXT_FILE)) {
	   call erract (EA_FATAL)
	}

	# Make sure there is an extension for the output file.
	call iki_init ()
	call iki_parse (image2, root, extn)
	if (extn[1] == EOS) {
	      call error (13, "Input image extension not specified")
	}
	if (strncmp (extn, "imh", LEN_EXTN) == 0)
	   call error (13, "Only GEIS files supported (not .imh files)")

	# Open output image on the local machine.
	iferr (im = immap (image2, NEW_IMAGE, 0)) {
	   call erract (EA_FATAL)
	}

	# Need to save image header file descriptor since gi_rheader
	# will use this value for the input header fd.
	save = IM_HFD(im)

	# Read header file and setup output image descriptors
	call gi_rheader (fdh, im)
	call close (fdh)
	IM_HFD(im) = save

	ngroups = gi_gstfval (im, "GCOUNT")
	npix = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	if (verbose) {
	   call printf ("%s(%dg,")
	      call pargstr (image1)
	      call pargi(ngroups)
	   do i = 1, IM_NDIM(im) {
	      call printf ("%d")
		  call pargi (IM_LEN(im,i))
	      if (i != IM_NDIM(im))  call printf ("x")
	   }
	   call printf (") --> %s\n")
	          call pargstr (image2)
	   call flush (STDOUT)
	}

	# Generate full input pixel file name.
	call iki_parse(image1, root, extn)
	call stf_mkpixfname (root, extn, pname, SZ_PATHNAME)

	pfd = open (pname, READ_ONLY, BINARY_FILE)

	bterm = (verbose && fstati(STDOUT,F_REDIR) == NO && 
		 nlines > 200)
	geisfile = YES
	if (bterm) call feed_user_begin(geisfile)

	call smark (sp)
	if (IM_PIXTYPE(im) == TY_SHORT || IM_PIXTYPE(im) == TY_USHORT)
	   call salloc (buf, npix, TY_SHORT)
	else if (IM_PIXTYPE(im) == TY_DOUBLE)
	   call salloc (buf, npix*2, TY_LONG)
	else
	   call salloc (buf, npix, TY_LONG)
	   
	# We set the flag below to prevent the stf kernel to create
	# a default gpb. The gpb information already exists from
        # the input file.
	call gi_pstfval (im, "NEWIMAGE", NO)
	
	# Next we need to preallocate disk space for the pixel file.
	# Normally this is done by stfopix, but the flag above prevent
	# this from happening.

	if (ngroups == 0)
	    ngroups = 1
	szgroup = gi_gstfval (im, "SZGROUP")
	sz_pixfile = szgroup * ngroups
	call falloc (IM_PIXFILE(im), sz_pixfile)

	oldtime = clktime(0)
	do gn = 1, ngroups {
	   # Setup start vector for sequential reads and writes.
	   call amovkl (long(1), v1, IM_MAXDIM)

	   if (gn > 1)
	     call gi_opengr (im, gn, datamin, datamax, 0)

	   switch (IM_PIXTYPE(im)) {
	   case TY_SHORT, TY_USHORT:
	       do nl = 1, nlines {
		  istat = read (pfd, Mems[buf], npix)
	          junk = impnls (im, pp, v1)
		  call bswap2 (Mems[buf], 1, Mems[pp], 1, npix*2)
		  if (bterm) call feed_user (gn, nl)
	       }
	   case TY_INT,TY_LONG:
	       do nl = 1, nlines {
	         junk = impnll (im, pp, v1)
		 istat = read (pfd, Meml[buf], npix*2)
		 call bswap4 (Meml[buf], 1, Meml[pp], 1, npix*4)
		 if (bterm) call feed_user (gn, nl)
	       }
	   case TY_REAL:
	       do nl = 1, nlines {
	         junk = impnlr (im, pp, v1)
		 istat = read (pfd, Meml[buf], npix*2)
		 call vx2sur (Meml[buf], Memr[pp], npix)
		 if (bterm) call feed_user (gn, nl)
	       }
	   case TY_DOUBLE:
	       do nl = 1, nlines {
	         junk = impnld (im, pp, v1)
		 istat = read (pfd, Meml[buf], npix*4)
		 call vx2sud (Meml[buf], Memd[pp], npix)
		 if (bterm) call feed_user (gn, nl)
	       }
	   default:
	       call error (1, "unknown pixel datatype")
	   }
	   # Print last column
	   if (bterm) call feed_last (gn, nlines)

	   call gi_rgpb_i3e (pfd, im, datamin, datamax)
	   IM_MIN(im) = datamin
	   IM_MAX(im) = datamax
	}
	if (bterm) call feed_user_end()

	call close (pfd)
	# Make sure we don't reset the last group data(min,max) to zero.
	IM_LIMTIME(im) = IM_MTIME(im) + 1

	# Reset flag for new_image so the new_image header can have
        # the input header info.

	call gi_pstfval (im, "NEWIMAGE", YES)
	call imunmap (im)
end
include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"iraf$sys/imio/iki/stf/stf.h"

# GF_RGPB_I3E -- A take from stf_rgpb to read gpb values from an image
# residing in a remote UNIX node. Each value is read and converted to vms
# floating point representation or to vms integer.
# Nelson Zarate Aug 1989
# Read the group data block into the first few cards of the user
# area of the IMIO image header.  The GPB is stored as a binary data structure
# in the STF pixfile.  The values of the standard GPB parameters DATAMIN and
# DATAMAX are returned as output arguments.
#

procedure gi_rgpb_i3e (pfd, im, datamin, datamax)

int	pfd			# Pixel file descriptor
pointer	im			# IMIO image descriptor
real	datamin, datamax	# min,max pixel values from GPB

long	offset
pointer	sp, stf, gpb, lbuf, pp
int	pn, sz_param, sz_gpb
real	imgetr()
int	read(), imaccf()
errchk	read

short	bufs
int	bufl

 
string	readerr "cannot read group data block"
string	badtype "illegal group data parameter datatype"
define	minmax_ 91

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	stf = IM_KDES(im)

	# Skip ahead if there is no group parameter block.
	if (STF_PSIZE(stf) == 0)
	    goto minmax_

	# Allocate a buffer for the GPB.
	sz_gpb = STF_PSIZE(stf) / NBITS_BYTE / SZB_CHAR
	call salloc (gpb, sz_gpb, TY_CHAR)

	# Read the GPB into a buffer.  The GPB is located at the very end of
	# the data storage area for the group.  If we are opening a new,
	# uninitialized group (acmode = new_image or new_copy), do not
	# physically read the GPB as it is will be uninitialized data.

	# Read directly from the pixel file since the offset is already 
	# done when reading the pixels???
        if (read (pfd, Memc[gpb], sz_gpb) != sz_gpb)
	    call error (1, readerr)

	# Extract the binary value of each parameter in the GPB and encode it
	# in FITS format in the IMIO user area.

	offset = 0
	for (pn=1;  pn <= STF_PCOUNT(stf);  pn=pn+1) {
	    pp = STF_PDES(stf,pn)
		
	    # Fill in the unitialized fields of the GPB parameter descriptor.
	    P_OFFSET(pp) = offset
	    sz_param = P_PSIZE(pp) / NBITS_BYTE / SZB_CHAR

	    switch (P_PDTYPE(pp)) {
	    # changed case for int to short and long--dlb 11/3/87
	    case 'I':
		if (sz_param == SZ_SHORT)
		    P_SPPTYPE(pp) = TY_SHORT
		else
		    P_SPPTYPE(pp) = TY_LONG
		P_LEN(pp) = 1
	    case 'R':
		if (sz_param == SZ_REAL)
		    P_SPPTYPE(pp) = TY_REAL
		else
		    P_SPPTYPE(pp) = TY_DOUBLE
		P_LEN(pp) = 1
	    case 'C':
		P_SPPTYPE(pp) = TY_CHAR
		# calculate length directly from PSIZE to avoid truncation error
		P_LEN(pp) = min (SZ_LINE, P_PSIZE(pp) / NBITS_BYTE)
	    case 'L':
		P_SPPTYPE(pp) = TY_BOOL
		P_LEN(pp) = 1
	    default:
		call error (1, badtype)
	    }

	    # Extract the binary parameter value and add a FITS encoded card
	    # to the IMIO user area.  In the case of a new copy image, the
	    # GPB values will already be in the image header, do not modify
	    # the parameter value, but add the parameter if it was not
	    # inherited from the old image.

	    switch (P_SPPTYPE(pp)) {
	    case TY_BOOL:
	       call bswap4 (Memc[gpb+offset], 1, bufl, 1, 4)
	       call imaddb (im, P_PTYPE(pp),  bufl)
	    # changed case for int to short and long--dlb 11/3/87
	    case TY_SHORT:
	       call bswap2 (Memc[gpb+offset], 1, bufs, 1, 2)
	       call imadds (im, P_PTYPE(pp), bufs)
	    case TY_LONG, TY_INT:
	       call bswap4 (Memc[gpb+offset], 1, bufl, 1, 4)
	       call imaddl (im, P_PTYPE(pp), bufl)
	    case TY_REAL:
	       call v2sr (Memc[gpb+offset])
	       call imaddr (im, P_PTYPE(pp),  Memc[gpb+offset])
	    case TY_DOUBLE:
	       call v2sd (Memc[gpb+offset])
	       call imaddd (im, P_PTYPE(pp), Memc[gpb+offset])
	    case TY_CHAR:
	       call chrupk (Memc[gpb+offset], 1, Memc[lbuf], 1, P_LEN(pp))
	       Memc[lbuf+P_LEN(pp)] = EOS
	       call imastr (im, P_PTYPE(pp), Memc[lbuf])
	    default:
	       call error (1, badtype)
	    }

	    offset = offset + sz_param
	}

minmax_
	# Return DATAMIN, DATAMAX.  This is done by searching the user area so
	# that ordinary keywords may be used to set datamin and datamax if the
	# GPB is not used.

	datamin = 0.0; datamax = 0.0
	if (imaccf (im, "DATAMIN") == YES) 
	    datamin = imgetr (im, "DATAMIN")
	if (imaccf (im, "DATAMAX") == YES) 
	    datamax = imgetr (im, "DATAMAX")
	call sfree (sp)
end
