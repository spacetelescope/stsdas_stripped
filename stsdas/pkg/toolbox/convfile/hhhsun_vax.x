include <imio.h>
include <imhdr.h>
include <fset.h>
include <error.h>

define	LEN_EXTN	3
# CPSUN_VAX -- Procedure to convert a SUN SDAS image into a VMS SDAS image.
# The input image should have UNIX floating point number (IEEE). It will
# convert any UNIX/IRAF datatype into a VMS/IRAF datatype; i.e. no consideration
# for small or big number is taken when converting.

procedure hhhsun_vax (image1, image2, verbose)

char	image1[SZ_FNAME], image2[SZ_FNAME]
bool	verbose

pointer im, buf, pp, sp
char	root[SZ_FNAME], extn[LEN_EXTN]
char	pname[SZ_PATHNAME]
int	v1[IM_MAXDIM]
int	fdh, pfd, istat
int	gn, npix, junk, szgroup
int	impnls(), impnlr(), impnll(), impnld()
int	open(), immap(), strncmp(), read()
int	gi_gstfval()

bool    bterm
int	fstati(), clktime(), nl

real	datamin, datamax
int	save, sz_pixfile, i

errchk  open, immap

include "feed_user.com"

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

	iferr (pfd = open (pname, READ_ONLY, BINARY_FILE)) {
	    call erract (EA_FATAL)
	}

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
	
	bterm = (verbose && fstati(STDOUT,F_REDIR) == NO && 
		 nlines > 200)
	geisfile = YES
	if (bterm) call feed_user_begin (geisfile)

	# Next we need to preallocate disk space for the pixel file.
	# Normally this is done by stfopix, but the flag above prevent
	# this from happening.

	szgroup = gi_gstfval (im, "SZGROUP")
	sz_pixfile = szgroup * ngroups

#	junk = open (IM_PIXFILE(im), NEW_FILE, BINARY_FILE)
#        call close(junk)
	call falloc (IM_PIXFILE(im), sz_pixfile)

	if (ngroups == 0)
	    ngroups = 1
	oldtime = clktime(0)
	do gn = 1, ngroups {
	   # Setup start vector for sequential reads and writes.
	   call amovkl (long(1), v1, IM_MAXDIM)

	   if (gn > 1)
	     call gi_opengr (im, gn, datamin, datamax, 0)

	   switch (IM_PIXTYPE(im)) {
	   case TY_SHORT, TY_USHORT:
	       do nl = 1, nlines {
	          junk = impnls (im, pp, v1)
		  istat = read (pfd, Mems[buf], npix)
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
#		 call sun2vaxr (Meml[buf], Memr[pp], npix)
		 call ieevupkr (Meml[buf], Memr[pp], npix)
		 if (bterm) call feed_user (gn, nl)
	       }
	   case TY_DOUBLE:
	       do nl = 1, nlines {
	         junk = impnld (im, pp, v1)
		 istat = read (pfd, Meml[buf], npix*4)
#		 call sun2vaxd (Meml[buf], Memd[pp], npix)
		 call ieevupkd (Meml[buf], Memd[pp], npix)
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
	call close (pfd)
        # Make sure we don't reset the last data(min,max) to zero.
	IM_LIMTIME(im) = IM_MTIME(im) + 1

	if (bterm) call feed_user_end()

	call sfree(sp)
	# Reset flag for new_image so the new_image header can have
        # the input header info.

	call gi_pstfval (im, "NEWIMAGE", YES)
	call imunmap (im)
end
