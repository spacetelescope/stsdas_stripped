include <math/curfit.h>
include <imhdr.h>
include <imio.h>
include "mkms.h"

# Define the extra characters needed for the units and labels.
# This is the length of the string:
define  PAD     "wtype=multispec label =  \"\" units = \"\""

# How many WAT keywords there can be.
define  MAX_HEADER      999

#---------------------------------------------------------------------------
.help mk_add_groups Mar93 source
.ih
NAME
mk_add_groups -- Check for and add MULTISPEC parameters to group parameters
.ih
USAGE
call mk_add_groups (in, n_wat2, label_length, function, out_name, max_chars)
.ih
ARGUMENTS
.ls in (pointer :input)
Input image descriptor.
.le
.ls n_wat2 (int :input)
The maximum number of WAT2_XXX keywords that will be needed.
.le
.ls label_length (int :input)
Length of the label and units strings combined.
.le
.ls function (int :input)
The fitting function used.  Need to know to determine whether a
table is being written.  If so, no attempt at creating multigroup parameters
will be made.
.le
.ls out_name (char[max_chars] :input/output)
The name of the new image file containing the original data plus the added
multigroup keywords.  If this has a value when called, the input data will
always be copied to the new image, whether parameters had to be added or not.
If this parameter is blank on input, then if it is blank on output, changes
had to be made.
.le
.ls max_chars (int :input)
Maximum number of characters allowed in the output file name out_name.
.le
.ih
DESCRIPTION
This routine adds the necessary keywords to multigroup data needed to
store the largest MULTISPEC parameters.  This routine only checks for
keyword existance, not whether the keyword is multigroup or not.  At
the moment, there is no way of checking whether a keyword is
multigroup or not.  If the keyword doesn't exist, it is added to the
group parameter block.  All MULTISPEC (as of v2.10 IRAF) parameters
can be multigroup except for the TABLE, which places a table in the
header.  There is a limit of 50 group parameters in the STF kernel,
which prevents tables from being multigroup.
.endhelp
#---------------------------------------------------------------------------
procedure mk_add_groups (in_image, n_wat2, label_length, function,
                         out_name, max_chars)

pointer in_image                # I:  Input image descriptor.
int     n_wat2                  # I:  Max number of WAT2_XXX keywords.
int     label_length            # I:  Size of label and units strings.
int     function                # I:  The function used in the fits.
char    out_name[max_chars]     # IO: The new output file.
int     max_chars               # I:  Maximum size of output file name.

bool    added                   # True if a group parameter is added.
int     fnextn()                # Get the extension of the file name.
int     i, j, k                 # Generic.
int     imaccf()                # Is the header present?
pointer immap()                 # Open the IMIO image.
bool    keep_out                # True always write the output image.
pointer out_image               # Output image descriptor.
real    rjunk                   # Generic.
pointer sp                      # Stack pointer.
int     strlen()                # Get length of string.
pointer xstr                    # Generic.

begin
        call smark(sp)
        call salloc (xstr, SZ_LINE, TY_CHAR)

        # If an output file is specified, always write to it.
        if (strlen (out_name) > 0)
            keep_out = true

        # Else...
        # Create the output image.  REMEMBER! Use the extension from
        # the input image because imcopy does not preserve this!
        else {
            keep_out = false
            call mktemp ("tmp$addmsp", out_name, max_chars)
            i = fnextn (IM_HDRFILE(in_image), Memc[xstr], SZ_LINE)
            call strcat (".", out_name, max_chars)
            call strcat (Memc[xstr], out_name, max_chars)
        }

        # Open the output and copy the first group.
        out_image = immap (out_name, NEW_COPY, in_image)
        call gf_pstfval (out_image, "GCOUNT", IM_CLSIZE(in_image))
        call grm_imcopy (in_image, out_image)
        
        # Now check for keywords.  If not present, add them to the group
        # parameter block.  This is done real dumb but there seems to be
        # no reason to make it more complicated.  Only do this if
        # the function is not a table.
        added = false

        if (function != MK_TABLE) {
            if (imaccf (out_image, "WCSDIM") == NO) {
                added = true
                j = TY_INT
                call gf_addpar (out_image, "WCSDIM", j, 1, "2", "")
            }
            
            if (imaccf (out_image, "CTYPE1") == NO) {
                added = true
                j = TY_CHAR
                call gf_addpar (out_image, "CTYPE1", j, 8, "MULTISPE", "")
            }
            
            if (imaccf (out_image, "CTYPE2") == NO) {
                added = true
                j = TY_CHAR
                call gf_addpar (out_image, "CTYPE2", j, 8, "MULTISPE", "")
            }
            
            if (imaccf (out_image, "CRVAL1") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "CRVAL1", j, 1, "0.0", "")
            }
            
            if (imaccf (out_image, "CRVAL2") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "CRVAL2", j, 1, "0.0", "")
            }
            
            if (imaccf (out_image, "CRPIX1") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "CRPIX1", j, 1, "0.0", "")
            }
            
            if (imaccf (out_image, "CRPIX2") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "CRPIX2", j, 1, "0.0", "")
            }
            
            if (imaccf (out_image, "CD1_1") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "CD1_1", j, 1, "1.0", "")
            }
            
            if (imaccf (out_image, "CD1_2") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "CD1_2", j, 1, "0.0", "")
            }
            
            if (imaccf (out_image, "CD2_1") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "CD2_1", j, 1, "0.0", "")
            }
            
            if (imaccf (out_image, "CD2_2") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "CD2_2", j, 1, "1.0", "")
            }
            
            if (imaccf (out_image, "LTM1_1") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "LTM1_1", j, 1, "1.0", "")
            }
            
            if (imaccf (out_image, "LTM2_2") == NO) {
                added = true
                j = TY_REAL
                call gf_addpar (out_image, "LTM2_2", j, 1, "1.0", "")
            }
            
            if (imaccf (out_image, "WAXMAP01") == NO) {
                added = true
                j = TY_CHAR
                call gf_addpar (out_image, "WAXMAP01", j, ATTR_LEN,
                                "1 0 0 0", "")
            }
            
            if (imaccf (out_image, "WAT0_001") == NO) {
                added = true
                j = TY_CHAR
                call gf_addpar (out_image, "WAT0_001", j, ATTR_LEN,
                                "system=multispec", "")
            }
            
            # Add keywords that will hold label and units.
            i = ((label_length + strlen (PAD)) / ATTR_LEN) + 1
            k = TY_CHAR
            do j = 1, i {
                call sprintf (Memc[xstr], SZ_LINE, "WAT1_%03.3d")
                call pargi (j)
                if (imaccf (out_image, Memc[xstr]) == NO) {
                    added = true
                    call gf_addpar (out_image, Memc[xstr], k, ATTR_LEN,
                                    "", "")
                }
            }
            
            if (imaccf (out_image, "WAT2_001") == NO) {
                added = true
                call gf_addpar (out_image, "WAT2_001", k, ATTR_LEN,
                                "wtype=multispec spec1=\"", "")
            }
            
            # All extra attribute keywords will hold the coefficients of the fits.
            if (n_wat2 > MAX_HEADER) {
                call sprintf (Memc[xstr], SZ_LINE, "Too many header keywords (%d) would be created")
                call pargi (n_wat2)
                call error (1, Memc[xstr])
            }
            do j = 2, n_wat2 {
                call sprintf (Memc[xstr], SZ_LINE, "WAT2_%03.3d")
                call pargi (j)
                if (imaccf (out_image, Memc[xstr]) == NO) {
                    added = true
                    call gf_addpar (out_image, Memc[xstr], k, ATTR_LEN,
                                    "", "")
                }
            }
        }
        
        # If any parameters were added, complete the image copy.  Else, just
        # delete the temporary one.
        if (added || keep_out) {
            do i = 2, IM_CLSIZE(in_image) {
                call gf_opengr (in_image, i, rjunk, rjunk, 0)
                call gf_opengr (out_image, i, rjunk, rjunk, in_image)
                call grm_imcopy (in_image, out_image)
            }
            call imunmap (out_image)
            call gf_opengr (in_image, 1, rjunk, rjunk, 0)
        } else {
            call imunmap (out_image)
            call imdelete (out_name)
            out_name[1] = EOS
        }
        
        # That's all folks.
        call sfree(sp)
end
#---------------------------------------------------------------------------
# End of mk_add_groups
#---------------------------------------------------------------------------
