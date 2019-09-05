include	<mach.h>
include	<ctype.h>
include <imio.h>
include	<imhdr.h>
include	<iraf77.h>


# UUIMFO -- Utility to get information on an open SDAS/GEIS file.
#	    The output vector 'vs' is the section specified in the
#	    input filename. It consists of 3 values per dimension;
#	    starting value, end value and step size.
#
#	?	?	   Subroutine created.
# Phil Hodge  24-Apr-1989  Change calling sequence of imparse.

procedure uuimfo (im_id, f77nme, group_number, vs, ve, step, istat)

pointer im_id			# image descriptor
%	character*(*)	f77nme
int	group_number		# cluster index (group number)
long	vs[IM_MAXDIM]		# image section use, start
long	ve[IM_MAXDIM]		# image section use, end
long	step[IM_MAXDIM]		# image section use, step size
int	istat
char	cluster[SZ_FNAME], ksection[SZ_LINE], section[SZ_LINE]
int	i, ip, cl_index, cl_size
long	x1[IM_MAXDIM], x2[IM_MAXDIM]

begin

	call imparse (IM_NAME(im_id),
		cluster, SZ_FNAME, ksection, SZ_LINE, section, SZ_LINE,
		cl_index, cl_size)

	if (section[1] == EOS) {
	   do i = 1, IM_NDIM(im_id) {
	      vs[i]   = 1
	      ve[i]   = IM_SVLEN(im_id,i)
	      step[i] = 1
	   }
	} else {	   
	   ip = 2
	   while (IS_WHITE(section[ip]))
	       ip = ip + 1

	   # Decode the section string, yielding the vectors X1, X2, and STEP.

	   for (i=1;  i <= IM_MAXDIM && section[ip] != ']';  i=i+1)
	       call im_decode_subscript (section, ip, x1[i], x2[i], step[i])

	   do i =1, IM_NDIM(im_id) {
	      if (x1[i] == MAX_LONG)
		 vs[i] = IM_SVLEN(im_id,i)
	      else if (x1[i] == 0)
		 vs[i] = 1
	      else
		 vs[i] = x1[i]

	      if (x2[i] == MAX_LONG)
		 ve[i] = IM_SVLEN(im_id,i)
	      else if (x2[i] == 0)
		 ve[i] = 1
	      else
		 ve[i] = x2[i]
	   }
	}
	group_number = cl_index
	call f77pak (IM_HDRFILE(im_id), f77nme, SZ_FNAME)
	istat = ER_OK
	return

end
