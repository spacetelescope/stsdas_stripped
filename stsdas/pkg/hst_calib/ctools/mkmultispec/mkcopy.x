include <imhdr.h>
include "mkms.h"

#---------------------------------------------------------------------------
.help mk_copy Mar93 source
.ih
NAME
mk_copy -- Add group parameters and create a copy to an output file if requested.
.ih
USAGE
call mk_copy (output_name, n_groups, fit, input)
.ih
ARGUMENTS
.ls output_name (char[ARB] :input)
The name of the current output image.  If the modifications are to be
done "in place", this should be the empty string.
.le
.ls n_groups (int :input)
The number of groups in the input image.  Used to determine whether group
parameters need to be added or not.  If greater than 1, then the MULTISPEC
keywords will be added as group parameters.
.le
.ls fit (pointer :input)
The fit descriptor.  Use this to retrieve information about the function
and order in order to calculate the number of group parameters that will
be needed.
.le
.ls input (pointer :input/output)
On input, this holds the input image descriptor.  On output, this
holds the image descriptor of the image that will be modified.  This
could be the same as the input, or if a copy was made, this will be
the copy.  If group parameters needed to be added, this will point to
the image with the new parameters.
.le
.ih
DESCRIPTION
This routine's main purpose is to add any necessary group parameters
to the image that will contain the MWCS.  If the image is supposed to
be operating in place, a copy to a new file is still done, but the old file
is deleted and the new file is renamed.  If groups parameters are not
to be added, then a simple copy is done.  However, if in place, nothing occurs.
.endhelp
#---------------------------------------------------------------------------
procedure mk_copy (output_name, n_groups, fit, input)

char    output_name[ARB]        # I:  Name of the output file
int     n_groups                # I:  Number of groups in input file
pointer fit                     # I:  The fit descriptor.
pointer input                   # IO: Image descriptor.

int     label_length            # Length of the labels.
pointer immap()                 # Open on IMIO file.
pointer input_name              # Original input name.
pointer mk_map()                # Open a generic file.
pointer new_out                 # New output name.
pointer output                  # Output file descriptor.
pointer sp                      # Stack pointer.
int     strlen()                # Get length of string.

errchk  imcopy, imdelete, immap, imunmap, mk_add_groups

begin
        # Memory.
        call smark (sp)
        call salloc (new_out, SZ_LINE, TY_CHAR)
        call salloc (input_name, SZ_LINE, TY_CHAR)

        # Compute total length of the label and units strings.
        label_length = strlen (FIT_LABEL(fit)) + strlen (FIT_UNITS(fit))
        
        # Add the group parameters if this is multigroup data.
        call strcpy (output_name, Memc[new_out], SZ_LINE)
        call strcpy (IM_HDRFILE(IO_FD(input)), Memc[input_name], SZ_LINE)
        if (n_groups > 1) {
            if (strlen (Memc[new_out]) > 0) {
                call mk_add_groups (IO_FD(input), FIT_LARGE(fit),
                                    label_length, FIT_INIT_FUNC(fit),
                                    Memc[new_out], SZ_LINE)
                call mk_unmap (input)
            } else {
                call mk_add_groups (IO_FD(input), FIT_LARGE(fit), label_length,
                                    FIT_INIT_FUNC(fit),
                                    Memc[new_out], SZ_LINE)
                if (strlen (Memc[new_out]) > 0) {
                    call mk_unmap (input)
                    call imdelete (Memc[input_name])
                    call imcopy (Memc[new_out], Memc[input_name])
                    call imdelete (Memc[new_out])
                }
                call strcpy (Memc[input_name], Memc[new_out], SZ_LINE)
            }
        }
        
        # This isn't multigroup,  Just copy the image if need be.
        else {
            if (strlen (Memc[new_out]) > 0){
                output = immap (Memc[new_out], NEW_COPY, IO_FD(input))
                call grm_imcopy (IO_FD(input), output)
                call imunmap (output)
            } else
                call strcpy (Memc[input_name], Memc[new_out], SZ_LINE)
            call mk_unmap (input)
        }

        # That's all folks.
        input = mk_map (Memc[new_out], READ_WRITE)
        
        call sfree (sp)
        
end
#---------------------------------------------------------------------------
# End of mk_copy
#---------------------------------------------------------------------------
