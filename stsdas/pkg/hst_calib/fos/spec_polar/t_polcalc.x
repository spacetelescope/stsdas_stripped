procedure t_polcalc ()

# POLCALC -- This routine calculates polarization spectra from Stokes IQUV
# spectra as contained in FOS c3h images.  The input Stokes IQUV spectra
# must be UNORMALIZED.
#
# Nov 93: H.A.Bushouse - Original implementation. (STSDAS v1.3.1)
#
# Task parameters
char	input[SZ_FNAME]		# Input image name
char	output[SZ_FNAME]	# Output image name
bool	verbose

# Local variables
pointer	iroot, iextn, oroot, oextn
pointer	ofbuf

bool	clgetb()
int	immap()
pointer	sp

begin

	# Allocate memory for image name strings
	call smark (sp)
	call salloc (iroot, SZ_FNAME, TY_CHAR)
	call salloc (oroot, SZ_FNAME, TY_CHAR)
	call salloc (iextn, SZ_FNAME, TY_CHAR)
	call salloc (oextn, SZ_FNAME, TY_CHAR)

	# Get input image name
	call clgstr ("input", input, SZ_LINE)
        call iki_init ()
	call iki_parse (input, Memc[iroot], Memc[iextn])
	if (Memc[iextn]==EOS) call strcat (".c3h", input, SZ_FNAME)

	# Get output image name
	call clgstr ("output", output, SZ_FNAME)
	call iki_parse (output, Memc[oroot], Memc[oextn])
	if (Memc[oextn]==EOS) call strcat (".c3h", output, SZ_FNAME)

	verbose = clgetb ("verbose")

	# Copy the input image into the output image
	call imcopy (input, output)
	if (verbose) {
	    call printf ("%s -> %s\n")
		 call pargstr (input)
		 call pargstr (output)
	    call flush (STDOUT)
	}

	# Open the output image
	ofbuf = immap (output, READ_WRITE, 0)

	# Recalculate the polarization spectra from the Stokes spectra
	call calcpol (ofbuf)

	# Close the output image
	call imunmap (ofbuf)

	# Free memory
	call sfree (sp)

end
