include	<imio.h>
include	<imhdr.h>
include	<iraf77.h>
include <syserr.h>

# UUIMCP -- Utility routine to copy the image descriptor from one
#	    existing file to a new one modifying the type and
# size of the new image.

procedure uuimcp (f77nam, dtype, naxis, dimen, t_imid, n_imid, istat)

%	character*(*) f77nam
int	dtype			# image data_type
int	naxis			# image dimensionnality
long	dimen[IM_MAXDIM]	# size in each dimension
pointer t_imid			# template image descriptor
pointer n_imid			# new image descriptor
int	istat

char 	imname[SZ_PATHNAME]

pointer immap()
int	errcode()
int	i
string	imhdr "imhdr"

begin
	istat = ER_OK
	# Verify image size and datatype operands.
	if (naxis < 1 || naxis > IM_MAXDIM)
	    istat = ER_IMBADNAXIS
	if (istat == ER_OK)
	    do i = 1, naxis
		if (dimen[i] < 1)
		    istat = ER_IMBADDIMEN
	if (istat == ER_OK)
	    if (dtype != TY_USHORT && dtype != TY_SHORT && dtype != TY_REAL
		&& dtype != TY_INT && dtype != TY_LONG  && dtype != TY_DOUBLE
		&& dtype != TY_COMPLEX)
		   istat = ER_IMBADTYPE
	if (istat != ER_OK) 
	    return

	# Convert character string to SPP string
	call f77upk (f77nam, imname, SZ_PATHNAME)

	iferr ( n_imid = immap (imname, NEW_COPY, t_imid)) {
	   istat = errcode ()
	   if (istat == SYS_IMSECTNEWIM)
		istat = ER_IMILLSEC
	   if (istat == SYS_IKIEXTN)
	        istat = ER_IMBADEXTN
	   if (istat == SYS_IKIOPEN)
  	       istat = ER_IMOPNEW
	   # reset pointer to zero to avoid passing back an invalid pointer
	   n_imid = 0
	   return
	}
	# Set pixel file characteristics
	call amovl (dimen, IM_LEN(n_imid,1), naxis)
	IM_NDIM(n_imid) = naxis
	IM_PIXTYPE(n_imid) = dtype

	IM_UPDATE(n_imid) = YES

	istat = ER_OK

	return

end
