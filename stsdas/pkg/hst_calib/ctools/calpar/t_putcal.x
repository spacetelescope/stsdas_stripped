include <ctype.h>
include	<imhdr.h>
include "calpar.h"

#---------------------------------------------------------------------------
.help putcal Jul97 source
.ih
NAME
putcal -- Apply changes found in a instrument pset to a set of images.
.ih
USAGE
putcal output keywords
.ih
DESCRIPTION
putcal reads the specified instrument-specific pset specified in
keywords and applies the new values to the images specified by the
'output' parameter.  The files are checked for consistency in
instrument.  Parameters that are left blank are not modified in any of
the files.
.endhelp
#---------------------------------------------------------------------------
procedure t_putcal

#
#  07 July 1997  M. De La Pena  Modified to ensure that if the input image
#                               is a FITS file, only the PHU is updated.

# Declarations
bool	add				# TRUE to add keywords to header.
int     i                               # Generic.
int     locext                          # Index for filename "FITS" match.
int     nextn                           # Number of characters in the extension.

bool    verbose                         # TRUE to print file names as processed.

pointer contents                        # Contents of the instrument pset.
pointer image                           # Image descriptor.
pointer instrument                      # Instrument name.
pointer list                            # Output image list descriptor.
pointer sp                              # Stack pointer.
pointer tmp_str                         # Generic.
pointer utmp_str                        # Temporary string.

string  badclose_err    "putcal: Cannot close image %s.\n"
string  badinstr_err    "putcal: Image %s uses instrument %s, not %s\n"
string  badopen_err     "putcal: Cannot open image %s\n"
string  badput_err      "putcal: Cannot modify parameter %s of image %s\n"
string  keywords_parname  "keywords"
string  instr_keyword   "INSTRUME"
string  instr_warn       "WARNING: putcal: No keyword %s in image %s, no instrument check\n"
string  nextfile_str    "putcal: Modifying %s\n"
string  nofile_err      "putcal: No output files specified"
string  nopar_err       "putcal: No calibration pset specified"
string  output_parname  "output"
string  verbose_par     "verbose"

# Function Prototypes
int     imtgetim(), imtlen(), strdic(), strlen(), strsearch(), fnextn()
bool    clgetb(), is_empty(), streq()
pointer cp_open(), immap(), imtopenp()

errchk  cp_close, cp_open, imtclose, imtgetim, imtlen, imtopenp, salloc, smark

begin
        call smark (sp)
        call salloc (instrument, SZ_LINE, TY_CHAR)
        call salloc (tmp_str, SZ_LINE, TY_CHAR)
        call salloc (utmp_str, SZ_LINE, TY_CHAR)
        
        # Open the image list.
	list = imtopenp (output_parname)
	if (imtlen (list) == 0)
            call error (1, nofile_err)

        # Retrieve pset and determine which instrument we should be
	# dealing with.
        call clgstr (keywords_parname, Memc[tmp_str], SZ_LINE)
        if (strlen (Memc[tmp_str]) <= 0)
            call error (1, nopar_err)
        contents = cp_open (Memc[tmp_str])
        Memc[instrument] = EOS
        do i = 1, CP_NPARAMS(contents)
            if (streq (INSTR_PAR, CP_PAR(contents,i)))
                call strcpy (CP_VALUE(contents,i), Memc[instrument], SZ_LINE)

	# Get other parameters
	add = clgetb ("add")
        verbose = clgetb (verbose_par)

        # Now go through each image and apply the new values.
	while (imtgetim(list, Memc[tmp_str], SZ_FNAME) != EOF) {

            # Check if the image is a FITS file.  If it is, ensure that only 
            # the PHU extension has been appended to the file name for proper
            # acquisition of information.
            call imgcluster (Memc[tmp_str], Memc[tmp_str], SZ_LINE)
            nextn  = fnextn (Memc[tmp_str], Memc[utmp_str], SZ_LINE)
            call strupr (Memc[utmp_str])
            locext = strsearch (Memc[utmp_str], "FITS")
            if (locext != 0) 
                call strcat ("[0]", Memc[tmp_str], SZ_LINE)

            # Send information to STDOUT.
            if (verbose) {
                call printf (nextfile_str)
                call pargstr (Memc[tmp_str])
                call flush (STDOUT)
            }

            # Open the image.
            iferr (image = immap (Memc[tmp_str], READ_WRITE, 0)) {
                call eprintf (badopen_err)
                call pargstr (Memc[tmp_str])
                next
            }
            
            # Check that we are dealing with the same instrument.
            iferr (call imgstr (image, instr_keyword, Memc[tmp_str], SZ_LINE)) {
                call eprintf (instr_warn)
                call pargstr (IM_HDRFILE(image))
                call pargstr (instr_keyword)
            } else {
                call strlwr (Memc[tmp_str])
                if (strlen (Memc[instrument]) > 0 &&
                    !streq (Memc[tmp_str], Memc[instrument])) {
                    call eprintf (badinstr_err)
                    call pargstr (IM_HDRFILE(image))
                    call pargstr (Memc[tmp_str])
                    call pargstr (Memc[instrument])
                    next
                }
            }
            
            # Apply all the changes.
            do i = 1, CP_NPARAMS(contents) {
                if (strdic (CP_PAR(contents,i), Memc[tmp_str], SZ_LINE,
                            BAD_PARS) > 0)
                    next
                
                if (is_empty (CP_VALUE(contents,i)))
                    next

                if (strdic (CP_VALUE(contents,i), Memc[tmp_str], SZ_LINE,
                            UPPER_STR_DIC) > 0)
                    call strupr (CP_VALUE(contents,i))

		if (add) {
		    iferr (call imastr (image, CP_PAR(contents,i),
					CP_VALUE(contents,i))) {
			call eprintf (badput_err)
			call pargstr (CP_PAR(contents,i))
			call pargstr (IM_HDRFILE(image))
			next
		    }
		} else {
		    iferr (call impstr (image, CP_PAR(contents,i),
					CP_VALUE(contents,i))) {
			call eprintf (badput_err)
			call pargstr (CP_PAR(contents,i))
			call pargstr (IM_HDRFILE(image))
			next
		    }
		}
            }
            
            # Close up.
            iferr (call imunmap (image)) {
                call eprintf (badclose_err)
                call pargstr (IM_HDRFILE(image))
            }
	}

        # That's all folks.
        call cp_close (contents)
	call imtclose (list)
end
#---------------------------------------------------------------------------
# End of t_putcal
#---------------------------------------------------------------------------
# is_empty -- TRUE if the string contains only whitespace.
#---------------------------------------------------------------------------
bool procedure is_empty (input)

char    input[ARB]

# Declarations
int     i                       # Generic.

bool    result                  # TRUE if string is empty.

# Function prototypes.
int     strlen()

begin
        for (i = strlen (input); i > 0; i = i - 1)
            if (!IS_WHITE(input[i]))
                break

        result = (i <= 0)

        return (result)
end
#---------------------------------------------------------------------------
# End of is_empty
#---------------------------------------------------------------------------
