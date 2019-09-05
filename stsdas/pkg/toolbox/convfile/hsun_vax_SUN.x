include <imio.h>
include <imhdr.h>
include <fset.h>
include <error.h>

define  LEN_EXTN	3
define  HDR_TEMPLATE    "dev$pix.hhh"

# CPVAX_SUN -- Procedure to convert a VAX-SDAS image into a UNIX-SDAS
# image. IEEE convertion is done on the data and group parameter value.
# The output is written on the remote machine via iraf networking or
# it can be local for later transfer using FTP for example.

procedure hhhsun_vax (image1, image2, verbose)

char	image1[SZ_FNAME]
char	image2[SZ_FNAME]
bool	verbose

pointer im, fd, hfd, buf1, buf2, sp
int	immap(), v1[IM_MAXDIM]
int	gn, npix, nchars
char	root[SZ_FNAME], extn[LEN_EXTN], extn2[LEN_EXTN]
int	imgnls(), imgnll(), imgnlr(), imgnld()
char	line[SZ_LINE], temp[SZ_FNAME], node[SZ_FNAME]
int	getline(), strncmp(), nch
int	open(), strlen(), gi_gstfval(), stridx()
real	datamin, datamax

bool    bterm
int	fstati() , clktime()
int	current_line, i, szgroup

include "feed_user.com"

errchk  open, immap, fmkcopy

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


	ngroups = gi_gstfval (im, "GCOUNT")
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

	call fpathname (image2, temp, SZ_FNAME)
	# Get remote node name
	nch = stridx ("!", temp)
	call strcpy (temp, node, nch)
	call strcat (HDR_TEMPLATE, node, SZ_FNAME)

	iferr (call fmkcopy (node, image2))
		call erract (EA_FATAL)
	iferr (fd = open (image2, READ_WRITE, TEXT_FILE))
		call erract (EA_FATAL)

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

	szgroup = gi_gstfval (im, "SZGROUP")
	call falloc (image2, szgroup*ngroups)
	fd = open (image2, READ_WRITE, BINARY_FILE)

	bterm = (verbose && fstati(STDOUT,F_REDIR) == NO && 
		 nlines > 200)
	geisfile = YES  
	if (bterm) call feed_user_begin(geisfile)

	call smark (sp)
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
		      call feed_user(gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   case TY_USHORT:
	       nchars = npix
   	       while (imgnls (im, buf1, v1) != EOF) {
		   call bswap2 (Mems[buf1], 1, Mems[buf2], 1, nchars*2)
		   call write (fd, Mems[buf2], nchars)
		   if (bterm) {
		      call feed_user(gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   case TY_INT,TY_LONG:
	       nchars = npix*2
	       while (imgnll (im, buf1, v1) != EOF) {
		   call bswap4 (Meml[buf1], 1, Meml[buf1], 1, nchars*2)
		   call write (fd, Meml[buf1], nchars)
		   if (bterm) {
		      call feed_user(gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   case TY_REAL:
	       nchars = npix*2
	       while (imgnlr (im, buf1, v1) != EOF) {
		   call su2vxr (Memr[buf1], Meml[buf2], npix)
		   call write (fd, Meml[buf2], nchars)		  
		   if (bterm) {
		      call feed_user(gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   case TY_DOUBLE:
	       nchars = npix*4
	       while (imgnld (im, buf1, v1) != EOF) {
		   call su2vxd (Memd[buf1], Meml[buf2], npix)
		   call write (fd, Meml[buf2], nchars)		  
		   if (bterm) {
		      call feed_user(gn, current_line)
		      current_line = current_line + 1
		   }
	       }
	   default:
	       call error (1, "unknown pixel datatype")
	   }

	   # Print last column
	   if (bterm) {
	      current_line = nlines
	      call feed_last(gn, current_line)
	   }
	   call gi_wgpb_i3e (im, fd, gn)
	}

	if (bterm) call feed_user_end()

	call sfree (sp)
	call close (fd)

	call imunmap (im)
end

include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"iraf$sys/imio/iki/stf/stf.h"

# GF_WGPB_I3E -- A take from stf_wgpb to write the gpb values into
# a pixel file in ieee floating point format for the input
# vms floating point number or to UNIX integer order for the input
# vms integer numbers.
# Nelson Zarate Aug. 1989
# STF_WGPB -- Write the group parameter block data back into the pixel file.
# The GPB is described by a structure member list in the STF descriptor.
# The values of the GPB parameters are encoded as FITS cards in the user
# area of the IMIO descriptor.
#
# DLB--11/3/87: Made changes to allow i*2 and i*4 integer parameters in gpb.

procedure gi_wgpb_i3e (im, fd, group)

pointer	im			# IMIO image descriptor
pointer fd			# output file descriptor
int	group			# group to be accessed

long	offset
pointer	sp, stf, gpb, lbuf, pp, op
int	pfd, pn, sz_param, sz_gpb, i

int	strlen()
bool	bval, imgetb()
# changed to short and long for short integers in gpb
short	sval, imgets()
long	lval, imgetl()
#
real	rval, imgetr()
double	dval, imgetd()

string	writerr "cannot update group parameter block"
string	badtype "illegal group data parameter datatype"

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	stf = IM_KDES(im)
	pfd = STF_PFD(stf)

	# Not all images have group parameter blocks.
	if (STF_PSIZE(stf) == 0) {
	    call sfree (sp)
	    return
	}

	# Allocate a buffer for the GPB.
	sz_gpb = STF_PSIZE(stf) / NBITS_BYTE / SZB_CHAR
	call salloc (gpb, sz_gpb, TY_CHAR)

	# Extract the binary value of each parameter in the GPB and encode it
	# in FITS format in the IMIO user area.

	offset = 0
	for (pn=1;  pn <= STF_PCOUNT(stf);  pn=pn+1) {
	    pp = STF_PDES(stf,pn)
	    op = gpb + offset

	    # Fetch the value of the parameter from IMIO and write it into
	    # the GPB binary data structure.

	    switch (P_SPPTYPE(pp)) {
	    case TY_BOOL:
		iferr (bval = imgetb (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    bval = false
		}
		# Memb[(op-1)/SZ_BOOL+1] = bval
		call bswap4 (bval, 1, Memc[op], 1, 4)

	    # changed case for int to short and long 
	    # to allow i*2 in gpb--dlb 11/3/87
	    case TY_SHORT:
		iferr (sval = imgets (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    sval = 0
		}
		call bswap2 (sval, 1, Memc[op], 1, 2)

	    case TY_LONG, TY_INT:
		iferr (lval = imgetl (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    lval = 0
		}
		call bswap4 (lval, 1, Memc[op], 1, 4)

	    case TY_REAL:
		iferr (rval = imgetr (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    rval = 0.0
		}
		# Memr[(op-1)/SZ_REAL+1] = rval
		call su2vxr (rval, Memc[op], 1)

	    case TY_DOUBLE:
		iferr (dval = imgetd (im, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    dval = 0.0D0
		}
		# Memd[(op-1)/SZ_DOUBLE+1] = dval
		call su2vxd (dval, Memc[op], 1)

	    case TY_CHAR:
		# Blank fill the string buffer.
		do i = 1, P_LEN(pp)
		    Memc[lbuf+i-1] = ' '

		# Fetch the string value of the parameter.
		iferr (call imgstr (im, P_PTYPE(pp), Memc[lbuf], SZ_LINE))
		    call erract (EA_WARN)

		# Replace the EOS delimiter by a blank.
		i = strlen (Memc[lbuf])
		Memc[lbuf+i] = ' '

		# Pack the blank filled array into the GPB.
		call chrpak (Memc[lbuf], 1, Memc[gpb+offset], 1, P_LEN(pp))

	    default:
		call error (1, badtype)
	    }

	    sz_param = P_PSIZE(pp) / NBITS_BYTE / SZB_CHAR
	    offset = offset + sz_param
	}

	# Write the GPB into the pixfile.  The GPB is located at the very end
	# of the data storage area for the group.

	iferr {
	    call write (fd, Memc[gpb], sz_gpb)
	} then
	    call error (5, writerr)

	call sfree (sp)
end
