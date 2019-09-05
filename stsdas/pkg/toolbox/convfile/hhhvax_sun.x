include <imio.h>
include <imhdr.h>
include <fset.h>
include <error.h>

define  LEN_EXTN	3
# CPVAX_SUN -- Procedure to convert a VAX-SDAS image into a UNIX-SDAS
# image. IEEE convertion is done on the data and group parameter value.
# The output is written on the remote machine via iraf networking or
# it can be local for later transfer using FTP for example.

procedure hhhvax_sun (image1, image2, verbose)

char	image1[SZ_FNAME]
char	image2[SZ_FNAME]
bool	verbose

pointer im, fd, hfd, buf1, buf2, sp
int	immap(), v1[IM_MAXDIM]
int	gn, npix, nchars
char	root[SZ_FNAME], extn[LEN_EXTN], extn2[LEN_EXTN]
int	imgnls(), imgnll(), imgnlr(), imgnld()
char	line[SZ_LINE]
int	getline(), strncmp()
int	open(), strlen(), gi_gstfval()
real	datamin, datamax

bool    bterm
int     current_line, i, fstati(), clktime()

include "feed_user.com"

errchk   open, immap

begin

	call iki_init()
	call iki_parse(image1, root, extn)

	if (extn[1] == EOS) {
	      call error (13, "Input image extension not specified")
	}
	if (strncmp (extn, "imh", LEN_EXTN) == 0) {
	   call printf ("Warning: Only GEIS files supported (not .imh files)")
	   return
	}       

	iferr (im = immap (image1, READ_ONLY, 0)) {
	     call erract (EA_FATAL)
	}

	call iki_parse(image2, root, extn2)
	# see if directory name only
	if (strlen(root) == 0)
	   call strcat (image1, image2, SZ_FNAME)
	else
	   call iki_mkfname (root, extn, image2, SZ_FNAME)

	ngroups = gi_gstfval(im, "GCOUNT")

	if (ngroups == 0)
	   ngroups = 1
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

	iferr (fd = open (image2, NEW_FILE, TEXT_FILE)) {
            call erract (EA_FATAL)
        }

	# The stf kernel now closes the header file after it has being
	# copied to the IM_USERAREA.
	hfd = open (IM_HDRFILE(im), READ_ONLY, TEXT_FILE)

	while (getline (hfd, line, SZ_LINE) != EOF)
	     call putline (fd, line, SZ_LINE)

	call close (hfd)
	call close (fd)


	npix = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	
	extn[3] = 'd'
	call iki_mkfname (root, extn, image2, SZ_FNAME)
	iferr (fd = open (image2, NEW_FILE, BINARY_FILE)) {
             call erract (EA_FATAL)
	}

	bterm = (verbose && fstati(STDOUT,F_REDIR) == NO && 
		 nlines > 200)
	geisfile = YES
	if (bterm) call feed_user_begin (geisfile)

	call smark(sp)
	if (IM_PIXTYPE(im) == TY_USHORT || IM_PIXTYPE(im) == TY_SHORT)
	   call salloc (buf2, npix, TY_SHORT)
	else if (IM_PIXTYPE(im) == TY_DOUBLE)
	   call salloc (buf2, npix*2, TY_LONG)
	else
	   call salloc (buf2, npix, TY_LONG)

	oldtime = clktime(0)
	do gn=1, ngroups {

	   if (ngroups > 1)
	      call gi_opengr (im, gn, datamin, datamax, 0)
	   # Setup start vector for sequential reads and writes.
	   call amovkl (long(1), v1, IM_MAXDIM)
	   current_line = 1
	   switch (IM_PIXTYPE(im)) {

	   case TY_SHORT:
	       nchars = npix
   	       while (imgnls (im, buf1, v1) != EOF) {
		   call bswap2 (Mems[buf1], 1, Mems[buf2], 1, nchars*2)
		   call write (fd, Mems[buf2], nchars)
		   if (bterm) {
		      call feed_user (gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   case TY_USHORT:
	       nchars = npix
   	       while (imgnls (im, buf1, v1) != EOF) {
		   call bswap2 (Mems[buf1], 1, Mems[buf2], 1, nchars*2)
		   call write (fd, Mems[buf2], nchars)
		   if (bterm) {
		      call feed_user (gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   case TY_INT,TY_LONG:
	       nchars = npix*2
	       while (imgnll (im, buf1, v1) != EOF) {
		   call bswap4 (Meml[buf1], 1, Meml[buf1], 1, nchars*2)
		   call write (fd, Meml[buf1], nchars)
		   if (bterm) {
		      call feed_user (gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   case TY_REAL:
	       nchars = npix*2
	       while (imgnlr (im, buf1, v1) != EOF) {
#		   call vx2sur (Memr[buf1], Meml[buf2], npix)
		   call ieevpakr (Memr[buf1], Meml[buf2], npix)
		   call write (fd, Meml[buf2], nchars)		  
		   if (bterm) {
		      call feed_user (gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   case TY_DOUBLE:
	       nchars = npix*4
	       while (imgnld (im, buf1, v1) != EOF) {
#		   call vx2sund (Memd[buf1], Meml[buf2], npix)
		   call ieevpakd (Memd[buf1], Meml[buf2], npix)
		   call write (fd, Meml[buf2], nchars)		  
		   if (bterm) {
		      call feed_user (gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   default:
	       call error (1, "unknown pixel datatype")
	   }
	   # Print last column
	   if (bterm)  call feed_last (gn, nlines)

	   call gi_wgpb_i3e (im, fd, gn)
	}

	if (bterm) call feed_user_end()

	call sfree(sp)
	call close (fd)
	call imunmap (im)
end
