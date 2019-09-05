include <imio.h>
include	<imhdr.h>
include "iraf$sys/imio/iki/stf/stf.h"

# UUOPGR  --  Procedure to skip onto the next group of a geis image
#	       already open. The group parameter block information
#	       is in memory for the previous group and it will be
#	       necessary only to write the gpb to the image (if the
#	       access mode is other than READ_ONLY) pixel file.
#	       If access is NEW_IMAGE we need to skip only after 
#	       the gpb has been written back to the file.

procedure uuopgr (im, gn, datamin, datamax, imt, ier)

pointer	im		# i: image descriptor
int	gn		# i: group number to skip to
real	datamin		# i,o: image minimun value
real	datamax		# i,o: image maximum value
pointer imt		# i: image template (for NEW_COPY)
int	ier		# o: error status

int	stf
int	compress, blklen, pixoff

bool	uugeis()
int	sizeof()

begin

	ier = 0
	compress = YES
	blklen = 1
	# Return if image is not geis file
	if (! uugeis (im)) {
	   ier = 27
	   # call error (13, "Image is not geis type")
	   return
	}

	stf = IM_KDES(im)

	if (gn > STF_GCOUNT(stf)) {
	   ier = 28
	   #call error (13, "Group number requested > No of groups in image")
	   return
	}

	switch (IM_ACMODE(im)) {
	case READ_WRITE, WRITE_ONLY, APPEND:
	    # Flush any data from buffer
	    call imflush (im)

	    # Update the group parameter block. Write the gpb of the
	    # current open group back to the data file.
	    call stf_wgpb (im, STF_GROUP(stf), datamin, datamax)

	    # Rgpb is independent of imoff
	    call stf_rgpb (im, gn, READ_ONLY, datamin, datamax)
	    IM_MIN(im) = datamin
	    IM_MAX(im) = datamax
	case READ_ONLY:

	    call stf_rgpb (im, gn, READ_ONLY, datamin, datamax)
	    IM_MIN(im) = datamin
	    IM_MAX(im) = datamax
	
	case NEW_IMAGE:
	    # Flush any data from buffer
	    call imflush (im)
	 
	    # Update the group parameter block. Write the gpb of the
	    # current open group back to the data file. We don't
	    # need to read gpb for the 'gn' group since we are
	    # creating those values.
	    call stf_wgpb (im, STF_GROUP(stf), datamin, datamax)
	case NEW_COPY:
	    #
	    if (STF_NEWIMAGE(stf) == NO)
	       call stf_rgpb (im, gn, READ_ONLY, datamin, datamax)
	    else {
 	       # We need to copy the values of the gpb from the
	       # input image (imt descriptor) to the output image
	       # before jumping to the next group. Stfwgp copies
	       # the gpb values from the userarea to the pixel file
	       if (STF_PFD(stf) != NULL) {
		  call imflush(im)
	          call stf_wgpb (im, STF_GROUP(stf), datamin, datamax)
	       }
	       iferr (call uu_copyua (imt, im,ier) )
		    return	   
	    }
	default:
	    # open an existing group within an existing image
	    call stf_rgpb (im, gn, READ_ONLY, datamin, datamax)
	    IM_MIN(im) = datamin
	    IM_MAX(im) = datamax
	}

	pixoff = (gn-1)*STF_SZGROUP(stf) + 1
	call imioff (im, pixoff, compress, blklen)
	if (mod(pixoff, sizeof(IM_PIXTYPE(im))) != 1)
	   IM_FAST(im) = NO

	# Update to the new group
	STF_GROUP(stf) = gn
end

include <syserr.h>
include	<iraf77.h>
include	<error.h>
include	<mach.h>

procedure uu_copyua (imt, im, ier)

pointer imt, im
int	ier

long	offset
pointer	sp, stf, lbuf, pp
#pointer gpb, op
int	pfd, pn, sz_param, i

bool	bval, imgetb()
# changed to short and long for short integers in gpb
short	sval, imgets()
long	lval, imgetl()
#
real	rval, imgetr()
double	dval, imgetd()
errchk	open, seek
int	errcode()
define  synerr_ 99

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

	# Extract the binary value of each parameter in the GPB and encode it
	# in FITS format in the IMIO user area.

	offset = 0
	for (pn=1;  pn <= STF_PCOUNT(stf);  pn=pn+1) {
	    pp = STF_PDES(stf,pn)
	    #op = gpb + offset

	    # Fetch the value of the parameter from IMIO and write it into
	    # the GPB binary data structure.

	    switch (P_SPPTYPE(pp)) {
	    case TY_BOOL:
		iferr (bval = imgetb (imt, P_PTYPE(pp))) {
		    bval = false
		    goto synerr_
		}
		call imputb (im, P_PTYPE(pp), bval)

	    # changed case for int to short and long 
	    # to allow i*2 in gpb--dlb 11/3/87
	    case TY_SHORT:
		iferr (sval = imgets (imt, P_PTYPE(pp))) {
		    sval = 0
		    goto synerr_
		}
		call imputs (im, P_PTYPE(pp), sval)
	    case TY_LONG:
		iferr (lval = imgetl (imt, P_PTYPE(pp))) {
		    lval = 0
		    goto synerr_
		}
		call imputl (im, P_PTYPE(pp), lval)
	    case TY_REAL:
		iferr (rval = imgetr (imt, P_PTYPE(pp))) {
		    rval = 0.0
		    goto synerr_
		}
		call imputr (im, P_PTYPE(pp), rval)

	    case TY_DOUBLE:
		iferr (dval = imgetd (imt, P_PTYPE(pp))) {
		    dval = 0.0D0
		    goto synerr_
		}
		call imputd (im, P_PTYPE(pp), dval)

	    case TY_CHAR:
		# Blank fill the string buffer.
		do i = 1, P_LEN(pp)
		    Memc[lbuf+i-1] = ' '

		# Fetch the string value of the parameter.
		iferr (call imgstr (imt, P_PTYPE(pp), Memc[lbuf], SZ_LINE))
		    goto synerr_

		# Replace the EOS delimiter by a blank.
		# i = strlen (Memc[lbuf])
		# Memc[lbuf+i] = ' '

		# Pack the blank filled array into the GPB.
		#call chrpak (Memc[lbuf], 1, Memc[gpb+offset], 1, P_LEN(pp))
		call impstr (im, P_PTYPE(pp), Memc[lbuf])

	    default:
		ier = ER_HDRPARTY
	        call sfree (sp)
		return
	    }
	    call flush (STDOUT)

	    sz_param = P_PSIZE(pp) / NBITS_BYTE / SZB_CHAR
	    offset = offset + sz_param
	}
	call sfree (sp)
	return
synerr_
	    ier = errcode()
	    if (ier == SYS_IDBKEYNF)
	       ier = ER_HDRPARNF
	    else
	       ier = ER_HDRPARTY 

end

bool procedure uugeis (im)

pointer	im		# i: Image descriptor
#--
pointer	stf

begin

	# Get stf descriptor. Compare several fields in the stf
	# descriptor to the image descriptor to make sure this is
	# a valid stf descriptor.

	stf = IM_KDES(im)
	if (stf == NULL)
	    return (false)

	if (IM_ACMODE(im) != STF_ACMODE(stf))
	    return (false)

	if (IM_NDIM(im) != STF_NAXIS(stf))
	    return (false)

	return (true)
end
