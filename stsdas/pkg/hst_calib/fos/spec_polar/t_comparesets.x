include <gset.h>
include	<imhdr.h>
include	<error.h>

procedure t_comparesets ()

pointer	fimage			# Input image name
pointer	wimage			# Wavelength image name
pointer	sample
pointer	device			# Device name string
int	nref			# Reference spectrum number
int	npass
bool	norm
bool	stats
real	xmin, xmax, ymin, ymax

pointer	gp			# graphics descriptor
pointer	im1, im2		# Image descriptor
pointer	wave			# Wavelengths
pointer flux			# Flux spectrum
pointer	root, extn, tmpc, tmp
int	npts, ngroup, tot_groups, off, roff, nsamp
int	first, middle, last, smin, smax
real	datamin, datamax, mean, sigma 
real	ybot, ytop, yint
pointer	title		# Plot title
pointer	xlabel, ylabel
bool	xautoscale, yautoscale

pointer	gopen(), sp
pointer	immap(), imgl1r(), imgeti()
int	i, clgeti(), imaccess()
real	clgetr()
bool	fp_equalr(), clgetb(), streq()

begin
	# Allocate memory
	call smark (sp)
	call salloc (fimage, SZ_FNAME, TY_CHAR)
	call salloc (wimage, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_LINE, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (tmpc, SZ_FNAME, TY_CHAR)
	call salloc (sample, SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (xlabel, SZ_LINE, TY_CHAR)
	call salloc (ylabel, SZ_LINE, TY_CHAR)

	# Get the data file name
	call clgstr ("image", Memc[fimage], SZ_FNAME)
        call iki_init ()
	call iki_parse (Memc[fimage], Memc[root], Memc[extn])
	if (streq(Memc[extn],""))
	    call strcat (".c1h", Memc[fimage], SZ_FNAME)

	# Find out which pass direction to plot
	npass = clgeti ("pass_dir")

	# Find out whether to plot absolute or normalized fluxes
	norm = clgetb ("normalize")

	# If normalizing, find out which position to use as reference
	if (norm) nref = clgeti ("refpos")

	# Find out whether to calculate statistics
	stats = clgetb ("stats")
	if (stats) call clgstr ("sample", Memc[sample], SZ_LINE)

	# Get the wavelength file name
        call clgstr ("wave", Memc[wimage], SZ_FNAME)
	if (Memc[wimage] == EOS)
	    call strcpy (Memc[root], Memc[wimage], SZ_FNAME)
	call iki_parse (Memc[wimage], Memc[root], Memc[extn])
	if (streq(Memc[extn],""))
	    call strcat (".c0h", Memc[wimage], SZ_FNAME)

	# Open the flux data file
	im2 = immap (Memc[fimage], READ_ONLY, 0)
	npts = IM_LEN(im2,1)
	tot_groups = imgeti (im2, "GCOUNT")

	# Allocate memory for the plot vectors
	call malloc (wave, npts, TY_REAL)
	call malloc (flux, npts*tot_groups/2, TY_REAL)
	if (norm) call malloc (tmp, npts, TY_REAL)

	# Get wavelength information
	if (imaccess(Memc[wimage], READ_ONLY) == YES) {
	    im1 = immap (Memc[wimage], READ_ONLY, 0)
	    call gf_opengr (im1, npass, datamin, datamax, 0)
	    call amovr (Memr[imgl1r(im1)], Memr[wave], npts)
	} else {
	    call printf(" Wavelength image %s not found; plot in pixel space\n")
		call pargstr (Memc[wimage])
		call flush (STDOUT)
	    do i = 1, npts
	       Memr[wave+i-1] = i
	}
	
	# Decode the statistics sample range parameter
	if (stats) {
	    nsamp = npts
	    iferr {
		call get_range (Memc[sample], Memr[wave], nsamp, smin, smax)
	    } then {
		call erract (EA_ERROR)
	    }
	}

	# Read the flux spectra
	do ngroup = npass, tot_groups, 2 {
	   call gf_opengr (im2, ngroup, datamin, datamax, 0)
	   off = npts * (ngroup-npass)/2
	   call amovr (Memr[imgl1r(im2)], Memr[flux+off], npts)
	}

	# Normalize the flux spectra by the reference spectrum
	if (norm) {
	    roff = npts * (nref-1)
	    do ngroup = 1, tot_groups/2 {
	       if (ngroup == nref) next
	       off = npts * (ngroup-1)
	       call adivr (Memr[flux+off],Memr[flux+roff],Memr[flux+off],npts)
	    }
	}

	# Get the user-specified window limits
	xautoscale = false
	yautoscale = false
	
	xmin = clgetr ("wx1")
	xmax = clgetr ("wx2")
	ymin = clgetr ("wy1")
	ymax = clgetr ("wy2")

	if (fp_equalr (xmin, xmax))
	    xautoscale = true
	if (fp_equalr (ymin, ymax))
	    yautoscale = true

	# Get the graphics device name
	call clgstr ("device", Memc[device], SZ_LINE)

	# Open the graphics device
	gp = gopen (Memc[device], NEW_FILE, STDGRAPH)
	call gswind (gp, xmin, xmax, ymin, ymax)

	# If autoscaling, find min/max of all spectra
	if (xautoscale)
	    call gascale (gp, Memr[wave], npts, 1)

	if (yautoscale) {
	    do ngroup = 1, tot_groups/2 {
	       if ((norm) && (ngroup==nref)) next
	       off = (ngroup-1)*npts
	       if (ngroup == 1)
		   call gascale (gp, Memr[flux+off], npts, 2)
	       else
		   call grscale (gp, Memr[flux+off], npts, 2)
	    }
	    call axmarg (gp)
	}
	call ggwind (gp, xmin, xmax, ymin, ymax)
	
	# Set up the plot characteristics and labels
	call gseti  (gp, G_YNMAJOR, 4)
	call gseti  (gp, G_YNMINOR, 0)
	call gsetr  (gp, G_YTICKLABELSIZE, 0.8)
	call gsetr  (gp, G_PLWIDTH, 0.5)

	ybot = 0.08
	ytop = 0.92
	yint = (ytop-ybot)/(tot_groups/2)
	if (norm) yint = (ytop-ybot)/(tot_groups/2-1)

	call strcpy ("Wavelength (A)", Memc[xlabel], SZ_LINE)
	if (Memr[wave] < 1000.) call strcpy ("Pixels", Memc[xlabel], SZ_LINE)
	call strcpy (Memc[fimage], Memc[title], SZ_LINE)
	call strcat (": pass dir = ", Memc[title], SZ_LINE)
	call itoc   (npass, Memc[tmpc], SZ_LINE)
	call strcat (Memc[tmpc], Memc[title], SZ_LINE)
	if (norm) {
	    call strcat ("\nNormalized to waveplate position ", Memc[title],
			 SZ_LINE)
	    call itoc   (nref, Memc[tmpc], SZ_LINE)
	    call strcat (Memc[tmpc], Memc[title], SZ_LINE)
	}

	# Plot the spectra
	first = 1
	if ((norm) && (nref==first)) first = 2
	middle = tot_groups/4
	if ((norm) && (nref<=middle)) middle=middle+1
	last = tot_groups/2
	if ((norm) && (nref==last)) last=last-1
	if (stats) 
	    call printf ("# POS  GROUP    SAMPLE       MEAN     STDDEV\n")
                          # xx     xx  [xxxx:xxxx]  x.xxxe-xx  x.xxxe-xx

	do ngroup = 1, tot_groups/2 {
	   if ((norm) && (ngroup == nref)) next
	   off = (ngroup-1)*npts
 	   call gsview (gp, 0.2, 0.95, ybot, ybot+yint)
	   call gswind (gp, xmin, xmax, ymin, ymax)
	   call itoc   (ngroup, Memc[ylabel], SZ_LINE)
	   if (ngroup == first)
	       call glabax (gp, "", Memc[xlabel], "")
	   else if (ngroup == middle)
	       call glabax (gp, "", "", "Waveplate Position")
	   else if (ngroup == last)
	       call glabax (gp, Memc[title], "", "")
	   else
	       call glabax (gp, "", "", "")
	   if (norm) 
	       call gtext (gp, xmin-0.12*(xmax-xmin), (ymin+ymax)/2.0, 
			   Memc[ylabel], "u=90;h=c;v=c;f=b;s=1.0")
	   else
	       call gtext (gp, xmin-0.14*(xmax-xmin), (ymin+ymax)/2.0, 
			   Memc[ylabel], "u=90;h=c;v=c;f=b;s=1.0")
	   call gpline (gp, Memr[wave], Memr[flux+off], npts)
	   call gseti  (gp, G_PLTYPE, GL_DASHED)
	   call gline  (gp, Memr[wave], 1.0, Memr[wave+npts-1], 1.0, npts)
	   call gseti  (gp, G_PLTYPE, GL_SOLID)
	   if (ngroup == first) call gseti (gp, G_XLABELTICKS, NO)
	   ybot = ybot + yint

	   # Calculate the mean and sigma
	   call aavgr (Memr[flux+off+smin-1], nsamp, mean, sigma)
	   call printf ("%4d %6d  [%4d:%4d] %10.4g %10.4g\n")
		call pargi (ngroup)
		call pargi (2*(ngroup-1)+npass)
		call pargi (smin)
		call pargi (smax)
		call pargr (mean)
		call pargr (sigma)
		call flush (STDOUT)
	}

	# Flush the graphics buffer
	call gflush (gp)

	# Close the images
	if (im1 != NULL) call imunmap (im1)
	call imunmap (im2)

	# Free memory
	call mfree (wave, TY_REAL)
	call mfree (flux, TY_REAL)
	if (norm) call mfree (tmp, TY_REAL)

	call sfree (sp)
	
	# Close graphics
	call gclose (gp)
end


include	<ctype.h>

# GET_RANGE -- decode range values from string

procedure get_range (rstr, rvals, npts, smin, smax)

char	rstr[ARB]		# Range string
real	rvals[npts]		# Range values (sorted)
int	npts			# Number of range values
int	smin, smax

int	i, j, k, strlen(), ctor()
real	rval1, rval2, a1, b1, a2, b2
pointer	sp, str, ptr

begin

	call smark (sp)
	call salloc (str, strlen (rstr), TY_CHAR)

	i = 1
	while (rstr[i] != EOS) {

	    # Find beginning and end of a range and copy it to the work string
	    while (IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n')
	        i = i + 1
	    if (rstr[i] == EOS)
		call error (1, "No sample range specified")

	    ptr = str
	    while (!(IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n' ||
		rstr[i]==EOS)) {
		if (rstr[i] == ':')
		    Memc[ptr] = ' '
		else
		    Memc[ptr] = rstr[i]
	        i = i + 1
		ptr = ptr + 1
	    }
	    Memc[ptr] = EOS

	    # Parse range
	    if (Memc[str] == '*') {
		rval1 = rvals[1]
		rval2 = rvals[npts]
	    } else {
		# Get range
		j = 1
		if (ctor (Memc[str], j, rval1) == 0)
		    call error (1, "Range syntax error")
		rval2 = rval1
		if (ctor (Memc[str], j, rval2) == 0)
		    ;
	    }

	    # Check limits and find indices into rval array
	    a1 = min (rval1, rval2)
	    b1 = max (rval1, rval2)
	    a2 = min (rvals[1], rvals[npts])
	    b2 = max (rvals[1], rvals[npts])

	    if ((b1 >= a2) && (a1 <= b2)) {
		a1 = max (a2, min (b2, a1))
		b1 = max (a2, min (b2, b1))
		if (rvals[1] <= rvals[npts]) {
		    for (k = 1; (k <= npts) && (rvals[k] < a1); k = k + 1)
			;
		    for (j = k; (j <= npts) && (rvals[j] <= b1); j = j + 1)
			;
		    j = j - 1
		} else {
		    for (k = 1; (k <= npts) && (rvals[k] > b1); k = k + 1)
			;
		    for (j = k; (j <= npts) && (rvals[j] >= a1); j = j + 1)
			;
		    j = j - 1
		}

		# Return range
		if (k <= j) {
		    smin = k
		    smax = j
		    npts = j - k + 1
		}
	    } else {
		call error (1, "No points in sample range")
	    }
	}

	call sfree (sp)
end
