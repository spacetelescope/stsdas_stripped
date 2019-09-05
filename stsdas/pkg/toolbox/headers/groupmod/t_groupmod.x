include <imhdr.h>
include <imio.h>
include "groupmod.h"

#---------------------------------------------------------------------------
.help groupmod Jul92 tools
.ih
NAME
groupmod -- Add/delete group parameters in multi-group images.
.endhelp
#---------------------------------------------------------------------------
procedure t_groupmod

# Declarations.
real    rjunk                   # Real junk.

int     i,j                     # Generic.
int     in_count                # Which input image is open now.
int     n_in                    # Number of input images.
int     n_out                   # Number of output file specifications.
int     op                      # Operation to perform.

bool    in_place                # Modifying the image in place.
bool    use_dir                 # Output specification is a directory.
bool    use_template            # Creating template-based image names.
bool    verbose                 # TRUE to print each image name as it is done.

pointer comments_par            # Name of column containing the comments.
pointer in_image                # Input image descriptor.
pointer in_list                 # Input image list descriptor.
pointer in_name                 # Curring input image name.
pointer initial_par             # Name of column containing the initial values.
pointer input_par               # Input image list.
pointer kw_par                  # Keyword table name and column specficiations.
pointer kw                      # Table descriptor for the kewords.
pointer names_par               # Name of column containing the parameter names.
pointer op_par                  # Operation string.
pointer out_image               # Output image descriptor.
pointer out_list                # Output list.
pointer out_name                # Output image name.
pointer output_par              # Output file name list.
pointer sp                      # Stack pointer.
pointer template                # Template output name.
pointer tmp_string              # Temporary string.
pointer types_par               # Name of column containing the parameter types.

# Function prototypes.
int     fnextn(), fnldir(), imtlen(), imtgetim(), isdirectory()
int     strdic(), strlen()
bool    clgetb(), imgetb()
pointer grm_open_kw(), immap(), imtopen()

begin
        call smark(sp)
        call salloc (comments_par, SZ_LINE, TY_CHAR)
        call salloc (in_name, SZ_LINE, TY_CHAR)
        call salloc (initial_par, SZ_LINE, TY_CHAR)
        call salloc (input_par, SZ_LINE, TY_CHAR)
        call salloc (kw_par, SZ_LINE, TY_CHAR)
        call salloc (names_par, SZ_LINE, TY_CHAR)
        call salloc (op_par, SZ_LINE, TY_CHAR)
        call salloc (out_name, SZ_LINE, TY_CHAR)
        call salloc (output_par, SZ_LINE, TY_CHAR)
        call salloc (template, SZ_LINE, TY_CHAR)
        call salloc (tmp_string, SZ_LINE, TY_CHAR)
        call salloc (types_par, SZ_LINE, TY_CHAR)
        
        # Retrieve the parameters.
        call clgstr ("input",     Memc[input_par],    SZ_LINE)
        call clgstr ("output",    Memc[output_par],   SZ_LINE)
        call clgstr ("keywords",  Memc[kw_par],       SZ_LINE)
        call clgstr ("operation", Memc[op_par],       SZ_LINE)
        call clgstr ("names",     Memc[names_par],    SZ_LINE)
        call clgstr ("types",     Memc[types_par],    SZ_LINE)
        call clgstr ("initial",   Memc[initial_par],  SZ_LINE)
        call clgstr ("comments",  Memc[comments_par], SZ_LINE)
        verbose = clgetb ("verbose")
        
        # Determine what operation is being done.
        op = strdic (Memc[op_par], Memc[op_par], SZ_LINE, OP_DICT)
        if (op == 0) {
            call sprintf (Memc[tmp_string], SZ_LINE, "Operation %s unknown, choose %s")
            call pargstr (Memc[op_par])
            call pargstr (OP_DICT)
            call error (1, Memc[tmp_string])
        }
        
        # Open the input image list.
        in_list = imtopen (Memc[input_par])
        n_in = imtlen (in_list)
        if (n_in == 0)
            call error (1, "No input images specified!")
        
        # Open the output image list.
        out_list = imtopen (Memc[output_par])
        n_out = imtlen (out_list)
        if (n_out == 0) {
            in_place = true
            use_template = false
            use_dir = false
        } else {
            in_place = false
            if (n_out == 1) {
                i = imtgetim (out_list, Memc[template], SZ_LINE)
                if (isdirectory (Memc[template], Memc[tmp_string],
                                 SZ_LINE) > 0) {
                    use_template = false
                    use_dir = true
                    call strcpy (Memc[tmp_string], Memc[template], SZ_LINE)
                } else if (nin > 1){
                    use_template = true
                    use_dir = false
                } else {
                    use_template = false
                    use_dir = false
                    call imtrew (out_list)
                }
            } else if (n_out != n_in)
                call error (1, "Number of output images should match number of input images")
            else {
                use_template = false
                use_dir = false
            }
        }
        
        # Open the keyword table and read in the necessary information.
        kw = grm_open_kw (Memc[kw_par], op, Memc[names_par], Memc[types_par],
                          Memc[initial_par], Memc[comments_par])
        
        # For each image, whack on the group parameters in the specified
        # fashion.
        do in_count = 1, n_in {
            
            # Open the input image.
            i = imtgetim (in_list, Memc[in_name], SZ_LINE)
            in_image = immap (Memc[in_name], READ_ONLY, 0)
            if (!imgetb (in_image, "GROUPS")) {
                call eprintf ("%s is not a multigroup file, skipping.\n")
                call pargstr (Memc[in_name])
                call imunmap (in_image)
                break
            }
            
            # Construct output file name.  Note- If "in_place", make sure the
            # temporary file has the same extension as the input file, or the
            # imcopy later on will give the "new" input file a default
            # extension.
            if (in_place) {
                call mktemp (TMP_ROOT, Memc[out_name], SZ_LINE)
                i = fnextn (Memc[in_name], Memc[tmp_string], SZ_LINE)
                call strcat (".", Memc[out_name], SZ_LINE)
                call strcat (Memc[tmp_string], Memc[out_name], SZ_LINE)
            } else if (use_dir) {
                call strcpy (Memc[template], Memc[out_name], SZ_LINE)
                i = strlen (Memc[out_name])
                j = fnldir (Memc[in_name], Memc[tmp_string], SZ_LINE)
                call strcat (Memc[in_name+j], Memc[out_name+i], SZ_LINE)
            } else if (use_template) {
                call strcpy (Memc[template], Memc[out_name], SZ_LINE)
                i = strlen (Memc[template])
                call sprintf (Memc[out_name+i], SZ_LINE-i, "%03.3d.hhh")
                call pargi (in_count)
            } else
                i = imtgetim (out_list, Memc[out_name], SZ_LINE)
            
            # Open output image and modify the parameters.
            out_image = immap (Memc[out_name], NEW_COPY, in_image)
            call gf_pstfval (out_image, "GCOUNT", IM_CLSIZE(in_image))
            call grm_imcopy (in_image, out_image)
            call grm_keyword_op (out_image, kw)

            # Now copy the rest of the groups and close up.
            do i = 2, IM_CLSIZE(in_image) {
                call gf_opengr (in_image, i, rjunk, rjunk, 0)
                call gf_opengr (out_image, i, rjunk, rjunk, in_image)
                call grm_imcopy (in_image, out_image)
            }
            call imunmap (out_image)
            call imunmap (in_image)

            # If desired, echo the files done.
            if (verbose) {
                call printf ("%s --> %s\n")
                call pargstr (Memc[in_name])
                if (in_place)
                    call pargstr (Memc[in_name])
                else
                    call pargstr (Memc[out_name])
            }

            # Finally, if doing this in-place, delete the original image
            # and copy the modified image.
            if (in_place) {
                iferr (call imdelete (Memc[in_name])) {
                    call sprintf (Memc[tmp_string], SZ_LINE,
                                  "Cannot delete original image %s, modified image is %s")
                    call pargstr (Memc[in_name])
                    call pargstr (Memc[out_name])
                    call error (1, Memc[tmp_string])
                } else  iferr (call imcopy (Memc[out_name], Memc[in_name])) {
                    call sprintf (Memc[tmp_string], SZ_LINE,
                                  "Cannot copy modified image %s to %s")
                    call pargstr (Memc[out_name])
                    call pargstr (Memc[in_name])
                    call error (1, Memc[tmp_string])
                } else iferr ( call imdelete (Memc[out_name])) {
                    call sprintf (Memc[tmp_string], SZ_LINE,
                                  "Cannot delete temporary image %s")
                    call pargstr (Memc[out_name])
                    call error (1, Memc[tmp_string])
                }
            }
        }

        # That's all folks.
        call grm_close_kw (kw)
        call imtclose (out_list)
        call imtclose (in_list)
        call sfree(sp)
        
end
#---------------------------------------------------------------------------
# End of t_groupmod
#---------------------------------------------------------------------------
procedure grm_imcopy (in, out)

pointer in                      # I:  Input image descriptor of image to copy.
pointer out                     # I:  Output image descriptor of resultant image.

# Declarations.
long    v1[IM_MAXDIM], v2[IM_MAXDIM]    # Line and section counters.

int     junk                            # Generic.
int     npix                            # Length of a line of data.

pointer buf1, buf2                      # Data buffers.

# Function Prototypes.
int	imgnls(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnll(), impnlr(), impnld(), impnlx()

begin
        
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)
        
	# Copy the image.
	npix = IM_LEN(in, 1)
	switch (IM_PIXTYPE(in)) {
	case TY_SHORT:
	    while (imgnls (in, buf1, v1) != EOF) {
		junk = impnls (out, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
	    }
	case TY_USHORT, TY_INT, TY_LONG:
	    while (imgnll (in, buf1, v1) != EOF) {
		junk = impnll (out, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
	    }
	case TY_REAL:
	    while (imgnlr (in, buf1, v1) != EOF) {
		junk = impnlr (out, buf2, v2)
		call amovr (Memr[buf1], Memr[buf2], npix)
	    }
	case TY_DOUBLE:
	    while (imgnld (in, buf1, v1) != EOF) {
		junk = impnld (out, buf2, v2)
		call amovd (Memd[buf1], Memd[buf2], npix)
	    }
	case TY_COMPLEX:
	    while (imgnlx (in, buf1, v1) != EOF) {
	        junk = impnlx (out, buf2, v2)
		call amovx (Memx[buf1], Memx[buf2], npix)
	    }
	default:
	    call error (1, "unknown pixel datatype")
	}
        
end
#---------------------------------------------------------------------------
# End of grm_imcopy
#---------------------------------------------------------------------------
