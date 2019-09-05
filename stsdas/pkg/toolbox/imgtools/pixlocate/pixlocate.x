include	<imhdr.h>

# pixlocate -- print positions of all points inside or outside a certain range
#
# Giaretta,		   Task created.
# Phil Hodge, 13-Feb-1990  Rename from marker to pixlocate.
# Phil Hodge, 19-Jun-1990  Upper border was off by one.
# Phil Hodge, 15-May-1995  Justify coordinates; print data values and header.

procedure t_pixlocate()

char	input[SZ_FNAME]		# input file name
real	lower_limit		# mark positions below this
real	upper_limit		# mark positions above this
int	maxvals			# max number to report
int	border			# border to miss at edges
bool	outside			# mark things outside or inside interval
#--
pointer sp
pointer cfmt, dfmt		# print formats for coordinates, data value
int	maxlen			# length of longest axis
int	clgeti(), pix, naxis1, count, ndims, i
real	clgetr()
long	v[IM_MAXDIM], old_v[IM_MAXDIM]
pointer	imgnlr(), immap, im, pt
bool	process, clgetb()
define	max_exceeded_ 99

begin
	call smark (sp)
	call salloc (cfmt, SZ_FNAME, TY_CHAR)
	call salloc (dfmt, SZ_FNAME, TY_CHAR)

	call clgstr ("input", input, SZ_FNAME)
	lower_limit = clgetr ("lowerlimit")
	upper_limit = clgetr ("upperlimit")
	maxvals = clgeti ("maxvals")
	border  = clgeti ("border")
	outside	= clgetb ("outside")

	im = immap (input, READ_ONLY, 0)
	ndims = IM_NDIM(im)
	call amovkl (long(1), v, IM_MAXDIM)
	naxis1 = IM_LEN(im, 1)
	maxlen = naxis1
	do i = 2, ndims
	    maxlen = max (maxlen, IM_LEN(im,i))

	# Print the beginning of the header.
	call printf ("#")

	# Set print format for coordinates.
	if (maxlen > 99999) {
	    call strcpy (" %10d", Memc[cfmt], SZ_FNAME)
	    do i = 1, ndims {
		call printf ("%9s%d ")
		    call pargstr ("x")
		    call pargi (i)
	    }
	} else if (maxlen > 9999) {
	    call strcpy (" %5d", Memc[cfmt], SZ_FNAME)
	    do i = 1, ndims {
		call printf ("%4s%d ")
		    call pargstr ("x")
		    call pargi (i)
	    }
	} else {
	    call strcpy (" %4d", Memc[cfmt], SZ_FNAME)
	    do i = 1, ndims {
		call printf ("%3s%d ")
		    call pargstr ("x")
		    call pargi (i)
	    }
	}

	# Set print format for data values.
	if (IM_PIXTYPE(im) == TY_INT || IM_PIXTYPE(im) == TY_LONG) {
	    call strcpy (" %10d", Memc[dfmt], SZ_FNAME)
	    call printf ("      data")
	} else if (IM_PIXTYPE(im) == TY_SHORT) {
	    call strcpy (" %6d", Memc[dfmt], SZ_FNAME)
	    call printf ("  data")
	} else if (IM_PIXTYPE(im) == TY_DOUBLE) {
	    call strcpy (" %20.14g", Memc[dfmt], SZ_FNAME)
	    call printf ("                data")
	} else {
	    call strcpy (" %14.6g", Memc[dfmt], SZ_FNAME)
	    call printf ("          data")
	}
	call printf ("  `%s'\n")
	    call pargstr (input)
	call flush (STDOUT)

	count = 0
	call amovl (v, old_v, ndims)
	while (imgnlr (im, pt, v ) != EOF) {
	    process = true
	    do i = 2, ndims
		if (old_v[i] > IM_LEN(im, i) - border || old_v[i] <= border)
		    process = false

	    if ( process ) {
		do pix = 1+border, naxis1-border {
		    if ( outside ) {
			if (  (!IS_INDEFR (lower_limit)  && 
			       Memr[pt+pix-1] <= lower_limit) ||
			      (!IS_INDEFR (upper_limit)  && 
			       Memr[pt+pix-1] >= upper_limit)) {
			    count = count + 1
			    if (!IS_INDEFI (maxvals) && count > maxvals) {
				call eprintf ("# maximum exceeded\n")
				goto max_exceeded_
			    }

			    # print coordinates - first axis special
			    call printf (Memc[cfmt])
				call pargi (pix)
			    do i = 2, ndims {
				call printf (Memc[cfmt])
				call pargi (old_v[i] )
			    }
			    call printf (Memc[dfmt])
				call pargr (Memr[pt+pix-1])
			    call printf ("\n")
			}
		    } else {
			if (  (IS_INDEFR (lower_limit)  || 
			       Memr[pt+pix-1] >= lower_limit)  &&
			      (IS_INDEFR (upper_limit)  || 
			       Memr[pt+pix-1] <= upper_limit)) {
			    count = count + 1
			    if (!IS_INDEFI (maxvals) && count > maxvals) {
				call eprintf ("# maximum exceeded\n")
				goto max_exceeded_
			    }

			    # print coordinates - first axis special
			    call printf (Memc[cfmt])
				call pargi (pix)
			    do i = 2, ndims {
				call printf (Memc[cfmt])
				call pargi (old_v[i] )
			    }
			    call printf (Memc[dfmt])
				call pargr (Memr[pt+pix-1])
			    call printf ("\n")
			}
		    }
		}
	    }    
	    call amovl (v, old_v, ndims)
	}

max_exceeded_
	call imunmap (im)
	call sfree (sp)
end
