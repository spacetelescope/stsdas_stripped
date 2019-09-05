include	<imhdr.h>
include <time.h>
include	"mka2d.h"

#  mka2d_do -- Generate the A-to-D reference file according to the option
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  04-Oct-1991  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mka2d_do (infile, outfile, errtable, option, b3temp)

char	infile[ARB], outfile[ARB]
char	errtable[ARB]
char	option[ARB]
real	b3temp

char	tmpfile[SZ_FNAME], gfile[SZ_FNAME]
real	errors[NBITS, NGROUP]
int	i, j, nrows, indx, hsize
char	camera[SZ_CAMERA], outstr[SZ_CAMERA]
char	tempstr[SZ_TEMP]
pointer	im, newim, ptr
pointer	sp, temp, mapping
pointer text
real	min, max, dtmin
bool	ieqo

bool	streq()
bool	strne()
pointer	impl2r(), imgl2r()
pointer	immap()
int	imgeti()
int	dtoc()
long    clktime()
#==============================================================================
begin

	# find out how long the history log may be
        hsize = SZ_TIME + SZ_FNAME + 50

	# convert bay 3 temperature to a string
	i = dtoc (double(b3temp), tempstr, SZ_TEMP, 2, 'f', SZ_TEMP)

	ieqo = false
	call smark (sp)
	call salloc (mapping, SZ_ATOD, TY_REAL)
        call salloc (text, hsize, TY_CHAR)
	call salloc (temp, SZ_ATOD, TY_REAL)

	# get the error table
	camera[1] = EOS
	if (streq (option, "new") || streq (option, "add"))
	    call get_err (errtable, errors, camera)
	
        call cnvtime (clktime(0), Memc[text], hsize)

	##############
	# NEW option #
	##############
	if (streq (option, "new")) {
	    iferr (im = immap (infile, READ_ONLY, 0))
		call error (1, "canot open the input file")
	    if (IM_NDIM(im) != 2 || IM_LEN(im, 1) != SZ_ATOD ||
		  IM_PIXTYPE(im) != TY_REAL || imgeti(im, "GCOUNT") != NGROUP)
		call error (1, "input file is not an A-to-D format file") 

	    call imgstr (im, "CAMERA", outstr, SZ_CAMERA)
	    if (strne (camera, outstr))
		call error (1, 
			"camera mismatch between input file and error table") 

	    # copy the input file to the output file
	    call strcat ("[1/4]", outfile, SZ_FNAME)
	    iferr (newim = immap (outfile, NEW_COPY, im))
		call error (1, "canot open the output file")
	    nrows = 2
	    IM_LEN (newim, 2) = nrows

	    # put the temperature (array) to the output file
	    do j = 1, SZ_ATOD
		Memr[temp+j-1] = -1.
	    Memr[temp] = 0.
	    Memr[temp+nrows-1] = b3temp

	    # build the new file
	    do i = 1, NGROUP {
		call gf_opengr (newim, i, min, max, im)
	        call amovr (Memr[temp], Memr[impl2r(newim, 1)], SZ_ATOD)

		# determine the reverse mapping of observed DN back to actual DN
	    	# and put the A-to-D correction (array) to the output file
	    	call mka2d_map (errors[1, i], Memr[mapping])
	    	call amovr (Memr[mapping], Memr[impl2r(newim, nrows)], SZ_ATOD)
	    }
            call strcat (" file created for error table '", Memc[text], hsize)
            call strcat (errtable, Memc[text], hsize)
            call strcat ("' at bay 3 temperature ", Memc[text], hsize)
            call strcat (tempstr, Memc[text], hsize)
            call imputh (newim, "HISTORY", Memc[text])
	}
	
	##############
	# ADD option #
	##############
	if (streq (option, "add")) {
	    iferr (im = immap (infile, READ_WRITE, 0))
		call error (1, "canot open the input file for read_write")
	    if (IM_NDIM(im) != 2 || IM_LEN(im, 1) != SZ_ATOD ||
		  IM_PIXTYPE(im) != TY_REAL || imgeti(im, "GCOUNT") != NGROUP)
		call error (1, "input file is not an A-to-D format file") 

	    call imgstr (im, "CAMERA", outstr, SZ_CAMERA)
	    if (strne (camera, outstr))
		call error (1, 
			"camera mismatch between input file and error table") 

	    if (streq (infile, outfile)) {
		ieqo = true
		call mktemp ("tmp", tmpfile, SZ_FNAME)
	    } else
		call strcpy (outfile, tmpfile)

	    call strcpy (tmpfile, gfile)
	    call strcat ("[1/4]", gfile, SZ_FNAME)
	    iferr (newim = immap (gfile, NEW_COPY, im))
		call error (1, "canot open the output file")
	    nrows = IM_LEN (im, 2) + 1
	    IM_LEN (newim, 2) = nrows

	    do i = 1, NGROUP {
		call gf_opengr (im, i, min, max, 0)
		call gf_opengr (newim, i, min, max, im)

		# copy the data
		do j = 2, nrows-1
	            call amovr (Memr[imgl2r(im, j)], Memr[impl2r(newim, j)], 
				SZ_ATOD)

	        # update the temperature (array) row
	        call amovr (Memr[imgl2r(im, 1)], Memr[temp], SZ_ATOD)
	        Memr[temp+nrows-1] = b3temp
	        call amovr (Memr[temp], Memr[impl2r(newim, 1)], SZ_ATOD)

		# determine the reverse mapping of observed DN back to actual DN
	    	# and put the A-to-D correction (array) to the output file
	    	call mka2d_map (errors[1, i], Memr[mapping])
	    	call amovr (Memr[mapping], Memr[impl2r(newim, nrows)], SZ_ATOD)
	    }
            call strcat (" one row added for error table '", Memc[text], hsize)
            call strcat (errtable, Memc[text], hsize)
            call strcat ("' at bay 3 temperature ", Memc[text], hsize)
            call strcat (tempstr, Memc[text], hsize)
            call imputh (newim, "HISTORY", Memc[text])
	}
	
	#################
	# DELETE option #
	#################
	if (streq (option, "delete")) {
	    iferr (im = immap (infile, READ_WRITE, 0))
		call error (1, "canot open the input file for read_write")
	    if (IM_NDIM(im) != 2 || IM_LEN(im, 1) != SZ_ATOD ||
		  IM_PIXTYPE(im) != TY_REAL || imgeti(im, "GCOUNT") != NGROUP)
		call error (1, "input file is not an A-to-D format file") 

	    if (streq (infile, outfile)) {
		ieqo = true	
		call mktemp ("tmp", tmpfile, SZ_FNAME)
	    } else
		call strcpy (outfile, tmpfile)

	    call strcpy (tmpfile, gfile)
	    call strcat ("[1/4]", gfile, SZ_FNAME)
	    iferr (newim = immap (gfile, NEW_COPY, im))
		call error (1, "canot open the output file")
	    nrows = IM_LEN (im, 2) - 1
	    if (nrows < 1) 
		call error (1, "input file has no A-to-D data")
	    IM_LEN (newim, 2) = nrows

	    do i = 1, NGROUP {
		call gf_opengr (im, i, min, max, 0)
		call gf_opengr (newim, i, min, max, im)
	        call amovr (Memr[imgl2r(im, 1)], Memr[temp], SZ_ATOD)

		# look for the index of the temperature to be removed
		if (i == 1) {
		    dtmin = abs(Memr[temp+1]-b3temp)
		    indx = 2
		    do j = 2, nrows+1 {
			if (abs(Memr[temp+j-1]-b3temp) < dtmin) {
		    	    dtmin = abs(Memr[temp+j-1]-b3temp)
			    indx = j
			}
		    }
		}	

	        # update the temperature (array) row
		do j = indx, nrows+1
		    Memr[temp+j-1] = Memr[temp+j]
	        call amovr (Memr[temp], Memr[impl2r(newim, 1)], SZ_ATOD)

		# delete the corresponding A-to-D array
		do j = 2, indx-1
	    	    call amovr (Memr[imgl2r(im, j)], 
				Memr[impl2r(newim, j)], SZ_ATOD)
		do j = indx, nrows
	    	    call amovr (Memr[imgl2r(im, j+1)], 
				Memr[impl2r(newim, j)], SZ_ATOD)
	    }
            call strcat (" deleted one row at bay 3 temperature ", 
				Memc[text], hsize)
            call strcat (tempstr, Memc[text], hsize)
            call imputh (newim, "HISTORY", Memc[text])
	}
	
	###############
	# LIST option #
	###############
	if (streq (option, "list")) {
	    iferr (im = immap (infile, READ_ONLY, 0))
		call error (1, "canot open the input file")
	    if (IM_NDIM(im) != 2 || IM_LEN(im, 1) != SZ_ATOD ||
		  IM_PIXTYPE(im) != TY_REAL || imgeti(im, "GCOUNT") != NGROUP)
		call error (1, "input file is not an A-to-D format file") 

	    call imgstr (im, "CAMERA", outstr, SZ_CAMERA)

	    # print the heading
	    call printf ("Bay 3 temperatures for the %s file %s:\n\n")
		call pargstr (outstr)
		call pargstr (infile)
	    call printf ("   Row    Bay 3 Temperature (K)\n")

	    # print the temperature (array) 
	    ptr = imgl2r(im, 1)
	    do j = 2, SZ_ATOD {
		if (nint(Memr[ptr+j-1]) == -1) 
		    break
		else {
		    call printf ("%5d      %8.2f\n")
			call pargi (j)
			call pargr (Memr[ptr+j-1])
		}
	    }
	}

	# close the input/output file 
	call imunmap (im)
	if (strne (option, "list")) 
	    call imunmap (newim)

	# if the output file is the same as input file, delete the input file,
	# and rename the output file to input file name
	if (ieqo) {
	    call imdelete (infile)
	    call imrename (tmpfile, outfile)
	}

	call sfree (sp)
end
