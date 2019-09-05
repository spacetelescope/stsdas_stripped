include <imhdr.h>
include "groupmod.h"

#---------------------------------------------------------------------------
.help grm_keyword_op Jul92 source
.ih
NAME
grm_keyword_op -- Perform the asked for operations on the keywords.
.endhelp
#---------------------------------------------------------------------------
procedure grm_keyword_op (im, kw)

pointer im                      # I:  The image descriptor.
pointer kw                      # I:  Keyword table structure.

# Declarations.
int     keyword                 # Counter into the keyword descriptor.

# Function Prototypes.
int     imaccf()

begin
        # Do the appropriate operations.
        switch (KW_OP(kw)) {
        case OP_ADD:
            do keyword = 1, KW_N_KEYWORDS(kw) {

                # If the keyword already exists, then say so, but continue on.
                if (imaccf (im, KW_NAME(kw,keyword)) == YES) {
                    call eprintf ("Keyword %s already exists in image %s, ignoring.\n")
                    call pargstr (KW_NAME(kw,keyword))
                    call pargstr (IM_HDRFILE(im))
                }

                # Else, add the keyword to the group.
                else {
                    iferr (call gf_addpar (im, KW_NAME(kw,keyword),
                                           KW_DTYPE(kw,keyword),
                                           KW_PLEN(kw,keyword),
                                           KW_INIT(kw,keyword),
                                           KW_COMM(kw,keyword))) {
                        call eprintf ("gf_addpar failed: could not add %s to image %s ... continuing\n")
                        call pargstr (KW_NAME(kw,keyword))
                        call pargstr (IM_HDRFILE(im))
                    }
                }
            }
            
        case OP_DEL:
            do keyword = 1, KW_N_KEYWORDS(kw) {

                # Check to see if the keyword exists.  If not, just say so.
                if (imaccf (im, KW_NAME(kw,keyword)) == NO) {
                    call eprintf ("Keyword %s doesn't exist in image %s, not deleting.\n")
                    call pargstr (KW_NAME(kw,keyword))
                    call pargstr (IM_HDRFILE(im))
                } else 
                    iferr (call gf_delpar (im, KW_NAME(kw,keyword))) {
                        call eprintf ("Could not delete keyword %s from image %s ... continuing\n")
                        call pargstr (KW_NAME(kw,keyword))
                        call pargstr (IM_HDRFILE(im))
                    }
            }
        }
        
        # That's all folks.
end
#---------------------------------------------------------------------------
# End of grm_keyword_op
#---------------------------------------------------------------------------
