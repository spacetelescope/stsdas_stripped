include <imhdr.h>
include <imio.h>
include <iraf77.h>

# Memory management.
define	Keyword		Memc[keyword]
define	Comment		Memc[comment]

#---------------------------------------------------------------------------
.help zadhis Apr94 source
.ih
NAME
zadhis -- Add history records to an image header.
.ih
DESCRIPTION
This add HISTORY records to an image header.  This needs to be packaged
because the imputh IRAF call has a bug.  The length of the  internal
header array is not increased.  The visible effect is that if the image
is used as a template to open other images, the added history records do
not appear.  See imputh for more information.

This routine is meant to be called by a FORTRAN level procedure.
.endhelp
#---------------------------------------------------------------------------
procedure zadhis (im, keyw, comm, ier)

pointer im		# I:  Image descriptor.
# The keyword to add.
%	character*(*) keyw
# The value of the keyword.
%	character*(*) comm
int	ier		# O:  Status return

# Declarations
pointer	comment		# The keyword value to add.
pointer	keyword		# The keyword to add.
pointer	sp		# Stack pointer

begin
	call smark (sp)
	call salloc (keyword, SZ_KEYWORD, TY_CHAR)
	call salloc (comment, SZ_CMDLINE, TY_CHAR)

	# Unpack the fortran string into SPP.
	call f77upk (keyw, Keyword, SZ_KEYWORD)
	call f77upk (comm, Comment, SZ_CMDLINE)

        # Update the header.
        iferr (call imputh (im, Keyword, Comment)) 
            ier = ER_HDRNOSP
        else {
            ier = ER_OK
            IM_UPDATE(im) = YES
        }

        # Now for the bug fix, extend the size of the header.
        IM_HDRLEN(im) = IM_HDRLEN(im) + SZ_CMDLINE

        # That's all folks.
        call sfree (sp)
end
