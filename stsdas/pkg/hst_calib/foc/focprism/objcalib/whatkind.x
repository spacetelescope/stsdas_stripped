include "objcalib.h"

# what_kind -- what kind of input file
# This routine opens the input file READ_ONLY, checking whether the file
# is an image or a table.  The appropriate pointer to use will be either
# im or tp, depending on whether the input file is an image or table
# respectively.  The other pointer will be set to NULL.
#
# Phil Hodge,  7-Apr-1993  Subroutine created.
# Phil Hodge,  2-Sep-1993  Modify calling sequence & eliminate text file.

procedure what_kind (input, im, tp, file_type)

char	input[ARB]		# i: name of input file
pointer im			# o: output pointer if file is an image
pointer tp			# o: output pointer if file is a table
int	file_type		# o: specifies image or table
#--
pointer immap()
pointer tbtopn()

begin
	# Check for an image.
	ifnoerr (im = immap (input, READ_ONLY, NULL)) {
	    tp = NULL
	    file_type = TYPE_IMAGE
	    return
	}

	# Check for a table.
	ifnoerr (tp = tbtopn (input, READ_ONLY, NULL)) {
	    im = NULL
	    file_type = TYPE_TABLE
	    return
	}

	# Neither of the above.
	im = NULL
	tp = NULL
	file_type = TYPE_UNKNOWN
end

