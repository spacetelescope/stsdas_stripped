include	<imhdr.h>

# T_SHARP - This task computes te sharpness for a list of stars in a image
#  It is a preliminary version.
#
# CYZhang 7 June 94  Based on Hashima's IDL code

procedure t_sharp()

pointer	sp			# Pointer to a stack
pointer	image			# Pointer to input image name
pointer	inlist			# Pointer to input table name
pointer	outlist			# Pointer to output table name
pointer	statsec			# Pointer to stat section
int	gn, cl_size, i
pointer	im, fd, fdout, tmp
int	xc, yc, boxsize
int	nstars, ip, npix, npts, nrows
pointer	str, sbuf, buf, sharp, brite
real	a, std
real	sum, sig2, backgrnd

int	fscan(), nscan(), open()
real	s_bckgrnd(), clgetr()
pointer	immap(), imgs2s()

begin

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (inlist, SZ_FNAME, TY_CHAR)
	call salloc (outlist, SZ_FNAME, TY_CHAR)	
	call salloc (statsec, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)
	# Get task parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("inlist", Memc[inlist], SZ_FNAME)
	call clgstr ("outlist", Memc[outlist], SZ_FNAME)
	call clgstr ("statsec", Memc[statsec], SZ_FNAME)

	boxsize = clgetr ("boxsize")

	# Get the cl_index for gn
	call rtparse (Memc[image], Memc[str], SZ_PATHNAME, gn, cl_size)
	if (gn == 0)
	    gn = 1
	# Map the input image, cl_index must be explicitly given otherwise
	# take the first group as default anyway
	im = immap (Memc[image], READ_ONLY, 0)
	npts = IM_LEN(im, 1)
	nrows = IM_LEN(im, 2)
	npix = npts * nrows

	fdout = open (Memc[outlist], NEW_FILE, TEXT_FILE)

	fd = open (Memc[inlist], READ_ONLY, TEXT_FILE)
	sbuf = imgs2s (im, 1, npts, 1, nrows)
	call malloc (buf, npix, TY_REAL)
	call achtsr (Mems[sbuf], Memr[buf], npix)

	backgrnd = s_bckgrnd (Memr[buf], npts, nrows, Memc[statsec])
	call asubkr (Memr[buf], backgrnd, Memr[buf], npix)
	nstars = 100
	call malloc (sharp, nstars, TY_REAL)
	call malloc (brite, nstars, TY_REAL)
	call fprintf (fdout, "#Sharpness for Image %s, Group No. %d, SKY= %g\n")
	call pargstr (Memc[str])
	call pargi (gn)
	call pargr (backgrnd)
	call fprintf (fdout, "#Star No.  Xc    Yc    Brightness Sharpness\n")
	ip = 0
	sum = 0.0
	while (fscan (fd) != EOF) {
	    ip = ip + 1
	    if (ip > nstars) {
		nstars = nstars + 100
		call realloc (sharp, nstars, TY_REAL)
		call realloc (brite, nstars, TY_REAL)	
	    }
	    call gargi (xc)
	    call gargi (yc)
	    if (nscan() < 2)
		call eprintf ("Need both X and Y")
	    call malloc (tmp, boxsize*boxsize, TY_REAL)
	    call s_center (Memr[buf], npts, nrows, xc, yc, Memr[tmp],
			   boxsize)
	    call s_sharp (Memr[buf], npts, nrows, xc, yc, Memr[tmp],
			  boxsize, Memr[brite+ip-1], Memr[sharp+ip-1])
	    call mfree (tmp, TY_REAL)
	    sum = sum + Memr[sharp+ip-1]
	    call fprintf (fdout, "%-10d %-6d%-6d%-10g %-10g\n")
	    call pargi (ip)
	    call pargi (xc)
	    call pargi (yc)
	    call pargr (Memr[brite+ip-1])
	    call pargr (Memr[sharp+ip-1])
	}
	if (ip <= 0) {
	    call eprintf ("No stars specified for group: %d")
	    call pargi (gn)
	} else {
	    a = sum / real(ip)
	    sig2 = 0.0
	    do i = 1, ip {
		sig2 = sig2 + (Memr[sharp+i-1] - a) ** 2
	    }
	    if (ip == 1) {
		std = sqrt (sig2 / real (ip))
	    } else
		std = sqrt (sig2 / real (ip - 1))
	}
	call fprintf (fdout, "#Mean Sharpness= %10g, RMS= %10g\n")
	call pargr (a)
	call pargr (std)
	
	call mfree (buf, TY_REAL)
	call close (fd)

	call imunmap (im)
	call close (fdout)
	call mfree (sharp, TY_REAL)
	call sfree (sp)

end

real procedure s_bckgrnd (data, nrows, npts, section)

real	data[npts, nrows]
int	nrows, npts
char	section[ARB]

int	v1[2], v2[2], vs[2], i, j, k, npix
pointer	work
real	backgrnd, amedr()

begin
	do i = 1, 2 {
	    v1[i] = 1
	    v2[i] = nrows
	    vs[i] = 1
	}
	call s_section (section, v1, v2, vs, 2)
	npix = ((v2[1] - v1[1]) / vs[1] + 1) * ((v2[2] - v1[2]) / vs[2] + 1)
	call malloc (work, npix, TY_REAL)
	do j = v1[2], v2[2], vs[2] {
	    do i = v1[1], v2[1], vs[1] {
		k = ((i - v1[1]) / vs[1] + 1) +
		    ((v2[1] - v1[1]) / vs[1] + 1) *
		    ((j - v1[2]) / vs[2])
		Memr[work+k-1] = data[i,j]
	    }
	}
	backgrnd = amedr [Memr[work], npix]
	call mfree (work, TY_REAL)
	return (backgrnd)

end

procedure s_sharp (data, npts, nrows, xc, yc, subdata, boxsize, brite, sharp)

real	data[npts, nrows]
real	brite, sharp
int	npts, nrows, xc, yc, boxsize, i, j
real	subdata[boxsize, boxsize]
real	sum, sum2

begin
	call s_gbox (data, npts, nrows, xc, yc, subdata, boxsize)
	sum = 0.0
	sum2 = 0.0
	do j = 1, boxsize {
	    do i = 1, boxsize {
		sum = sum + subdata[i,j]
		sum2 = sum2 + subdata[i,j]**2
	    }
	}
	if (sum <= 0.0){
	    call eprintf ("Star brightness is zero")
	    brite = 0.0
	    sharp = INDEFR
	} else {
	    brite = sum
	    sharp = sum2 / sum**2
	}
	
end	

procedure s_center (data, npts, nrows, xc, yc, subdata, boxsize)

real	data[npts, nrows]
int	npts, nrows, xc, yc, boxsize
real	subdata[boxsize, boxsize]

int	i, j, ipeak, jpeak

begin
	call s_gbox (data, npts, nrows, xc, yc, subdata, boxsize)
	ipeak = 1
	jpeak = 1
	do j = 1, boxsize {
	    do i = 1, boxsize {
		if (subdata[i,j] > subdata[ipeak, jpeak]) {
		    ipeak = i
		    jpeak = j
		}
	    }
	}
	xc = ipeak - boxsize / 2 + xc
	yc = jpeak - boxsize / 2 + yc

end


procedure s_gbox (data, npts, nrows, xc, yc, subdata, boxsize)

real	data[npts, nrows]
real	subdata[boxsize, boxsize]
int	npts, nrows, xc, yc, boxsize, halfbox
int	i, j, k1, k2
int	x1, x2, y1, y2

begin
	halfbox = boxsize / 2
	x1 = xc - halfbox
	x2 = xc + halfbox
	y1 = yc - halfbox
	y2 = yc + halfbox
	do j = y1, y2 {
	    do i = x1, x2 {
		k1 = i - x1 + 1
		k2 = j - y1 + 1
		subdata[k1,k2] = data[i,j]
	    }
	}
end
