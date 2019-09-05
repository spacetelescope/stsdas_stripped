include <iraf77.h>
include <imhdr.h>
include <imio.h>

define	LEN_CARD	80
# UUCPHB -- Utility to copy a block of image header from image1
#	    to image2 starting at keyword1 and finish at keyword2,
#           both included.

procedure uucphb (im1, im2, key1, key2, ier)

pointer	im1			# i: Input image descriptor
pointer im2			# i: Output image descriptor
 				# i: Initial keyword name to copy
%	character*(*) key1
				# i: Last keyword name to copy
%	character*(*) key2	
int	ier			# o: Error status

char	skey1[SZ_KEYWORD]
char	skey2[SZ_KEYWORD]	
int	idb_findrecord(), strlen()
int	pos1, pos2, rp1, rp2, nchar, ua1, ua2, sz_area2
int	new_size, ip

begin
	ier = ER_OK

	call f77upk (key1, skey1, SZ_KEYWORD)
	call f77upk (key2, skey2, SZ_KEYWORD)
	pos1 = idb_findrecord (im1, skey1, rp1)
	pos2 = idb_findrecord (im1, skey2, rp2)
	if (pos1 == 0 || pos2 == 0) {
	   ier = ER_HDRPARNF
	   return
	}	

	nchar = abs (rp2 - rp1) + LEN_CARD + 1

	ua1 = IM_USERAREA(im1)
	ua2 = IM_USERAREA(im2)
	# Realloc memory size for the new keywords.
	sz_area2 = strlen (Memc[ua2])
	new_size = IM_LENHDRMEM(im2) + LEN_IMDES + nchar
	if (new_size > ARB) {
	   ier = ER_HDRNOSP     # Cannot copy beyond 404 cards 
	   return
	}
	IM_HDRLEN(im2) = IM_HDRLEN(im2) + nchar
	IM_LENHDRMEM(im2) = IM_LENHDRMEM(im2) + nchar
	call realloc (im2, new_size, TY_STRUCT)

	# Copy nchar to the output IM_USERAREA.
	ip = rp1
	if (rp1 > rp2)
	   ip = rp2

	# The im2 can get changed after realloc, so redefine
	# ua2.
	ua2 = IM_USERAREA(im2)
	call strcpy (Memc[ip], Memc[ua2+sz_area2], nchar)

end
                                     
                                             