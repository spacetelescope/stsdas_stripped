include <imhdr.h>

procedure adaptive()

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# IDENTIFICATION
#   	program afi			version 1.00		910320
#	G.M.Richter ZIAP Sternwarte Babelsberg
#
# PURPOSE
#	adaptive filtering of images:
#	reduction of the noise without damaging the resolution.
#	cleaning (smoothing, low pass filtering) as well as structure enhancing
#	and detecting (by two types of high pass filtering: gradient and 
#	Laplace filter) is possible.
#
# ALGORITHM
#	The local signal-to-noise ratio as a function of decreasing resolution
#	is evaluated via the H-transform: mean gradients and curvatures over 
#	different scale lengths (obtained from the H-coefficients of different
#	order) are compared to the corresponding expectation values of the
#	noise. The order for which this signal-to-noise ratio exceeds a given
#	parameter K indicates the local resolution scale length of the signal
#	(dubbed: the point becomes significant at this order), and determines
#	the size of the impulse response of the filter at this point.
#
# KEYWORDS
#	IN_A	input frame
#	IN_B	input mask for noise statistics:
#		Only unmasked (mask value=0) pixels of the input frame are
#		used to estimate the noise statistics.
#		If 'NULL' is put in, no mask is used
#	OUT_A	output frame
#	AFITYP	type of adaptive filter:
#		'S'=smoothing
#		'G'=gradient filter
#		'L'=Laplace-filter
#	AFISHA	shape of impulse resonse:
#		'B'=box
#		'P'=pyramide
#	AFISIZ	maximal size of impulse response
#		(in regions of high resolution the actual size is smaller)
#	AFIK	threshold for significance
#	AFINOI  noise model:
#		'A'=additive noise is assumed
#		'P'=Poisson-noise is assumed
#
# NOTE
#	When the algorithm is finished some information on the noise 
#	statistics is put out on the terminal: the standard deviation and
#	the exspectation values of the gradients and the Laplace-terms at 
#	every order involved, and the number of pixels which became significant
#	on every order by the gradient and by the Laplace term respectively.
#	The rest pixels are set to the maximal size response.
#
#
#  Main module converted to spp for iraf by Phil Hodge, 1991 Nov 25.
#--------------------------------------------------------------------------

char	in[SZ_FNAME], out[SZ_FNAME], mt[SZ_FNAME]
char	ftype[SZ_FNAME], fshape[SZ_FNAME], fnoise[SZ_FNAME]
real	k
int	siz
#--
int	ord, naxis, npix[2]
pointer imnin, imnout, imnmt
pointer pntin, pntout, pnth0, pnth1, pnthg, pnthl, pntm, pntmt

%       CHARACTER TYP*1, FORM*1, NOI*1, YMT*1

real	cut[4]

pointer immap()
pointer imgs2r(), imps2r()
real	clgetr()
int	clgeti()

# common block for statistics
int	nsg, nsl, rest
real	eg, el, sg, sl, mog, mol, mul
common	/test/ eg(16), sg(16), el(16), sl(16), nsg(16), nsl(16),
		rest, mog, mol, mul

# *** include MIDAS definition table
#      INCLUDE       'MID_INCLUDE:ST_DEF.INC'
#      COMMON        /VMR/MADRID
#      INCLUDE       'MID_INCLUDE:ST_DAT.INC'

begin
	call clgstr ("input", in, SZ_FNAME)
	call clgstr ("output", out, SZ_FNAME)
	call clgstr ("mask", mt, SZ_FNAME)

	call clgstr ("type", ftype, SZ_FNAME)	# filter type
	call clgstr ("shape", fshape, SZ_FNAME)	# inpulse-response shape
	siz = clgeti ("size")			# filter size
	k = clgetr ("threshold")
	call clgstr ("noise", fnoise, SZ_FNAME)	# noise type

	call strupr (ftype)
	call strupr (fshape)
	call strupr (fnoise)

	# Convert to Fortran strings.

	if (ftype[1] == 'S')
%           typ = 'S'
	else if (ftype[1] == 'G')
%           typ = 'G'
	else if (ftype[1] == 'L')
%           typ = 'L'
	else
	    call error (1, "Invalid filter type")

	if (fshape[1] == 'B')
%           form = 'B'
	else if (fshape[1] == 'P')
%           form = 'P'
	else
	    call error (1, "Invalid impulse-response shape")

	if (fnoise[1] == 'A')
%           noi = 'A'
	else if (fnoise[1] == 'P')
%           noi = 'P'
	else
	    call error (1, "Invalid noise type")

	# open input frame
	imnin = immap (in, READ_ONLY, NULL)
	naxis = IM_NDIM(imnin)
	npix[1] = IM_LEN(imnin,1)
	npix[2] = IM_LEN(imnin,2)

	if (naxis.lt.2)
	    call error (1,'dimension less than 2')

	if (mt[1] == EOS || mt[1] == ' ') {
	    # no mask; create dummy pointer
	    call malloc (pntmt, 1, TY_REAL)
	    imnmt = NULL
%           YMT='N'
	} else {
	    imnmt = immap (mt, READ_ONLY, NULL)
	    if ((IM_LEN(imnmt,1) != npix[1]) || (IM_LEN(imnmt,2) != npix[2]))
		call error (1, 'Image and mask are not the same size')
%           YMT='Y'
	}

	# create output frame
	imnout = immap (out, NEW_COPY, imnin)

	# Get pointers to image data.
	pntin = imgs2r (imnin, 1, npix[1], 1, npix[2])
	if (imnmt != NULL)
	    pntmt = imgs2r (imnmt, 1, npix[1], 1, npix[2])
	pntout = imps2r (imnout, 1, npix[1], 1, npix[2])

	# *** get transform-order ORD from window-size SIZ
	if (fshape[1] == 'P') {		# pyramid
# ***   IF (FORM.EQ.'P') THEN
	    ORD = 2				# size = 3
	    IF(SIZ > 3)   ORD = 3
	    IF(SIZ > 5)   ORD = 4
	    IF(SIZ > 7)   ORD = 5
	    IF(SIZ > 11)  ORD = 6
	    IF(SIZ > 15)  ORD = 7
	    IF(SIZ > 23)  ORD = 8
	    IF(SIZ > 31)  ORD = 9
	    IF(SIZ > 47)  ORD = 10
	    IF(SIZ > 63)  ORD = 11
	    IF(SIZ > 95)  ORD = 12
	    IF(SIZ > 127) ORD = 13
	    IF(SIZ > 191) ORD = 14		# 255
	} else {			# box
	    ORD = 2				# size = 3
	    IF(SIZ > 3)   ORD = 3
	    IF(SIZ > 5)   ORD = 4
	    IF(SIZ > 9)   ORD = 5
	    IF(SIZ > 17)  ORD = 6
	    IF(SIZ > 33)  ORD = 7
	    IF(SIZ > 65)  ORD = 8
	    IF(SIZ > 129) ORD = 9
	    IF(SIZ > 257) ORD = 10		# 513
	}

	# *** create work space
	call malloc (pnth0, npix[1]*npix[2], TY_REAL)
	call malloc (pnth1, npix[1]*npix[2], TY_REAL)
	call malloc (pnthg, npix[1]*npix[2], TY_REAL)
	call malloc (pnthl, npix[1]*npix[2], TY_REAL)
	call malloc (pntm,  npix[1]*npix[2], TY_REAL)

	# *** start algorithm
	call afido (npix[1], npix[2], Memr[pntmt], Memr[pntin],
		Memr[pnth0], Memr[pnth1], Memr[pnthg],
		Memr[pnthl], Memr[pntout], Memr[pntm],
		ord, typ, form, noi, k, ymt)

	# *** information on image statistics and number of significant points
	call ainfo (ord)

	# *** write new descriptors
	call imastr (imnout, "afimask", mt)
	call imastr (imnout, "afityp", ftype)
	call imastr (imnout, "afisha", fshape)
	call imastr (imnout, "afinoi", fnoise)
	call imaddi (imnout, "afisiz", siz)
	call imaddr (imnout, "afik", k)

	# *** write cuts for gradient and laplace
	if (ftype[1] != 'S') {
	    if (ftype[1] == 'G') {		# gradient filter
		cut[2] = 3. * mog
		cut[1] = 0.
	    } else {				# Laplace filter
		cut[2] = 3. * mol
		cut[1] = 3. * mul
	    }
	    call imaddr (imnout, "lhcuts1", cut[1])
	    call imaddr (imnout, "lhcuts2", cut[2])
	}

	# *** delete work arrays
	call mfree (pnth0, TY_REAL)
	call mfree (pnth1, TY_REAL)
	call mfree (pnthg, TY_REAL)
	call mfree (pnthl, TY_REAL)
	call mfree (pntm,  TY_REAL)

	# close images
	if (imnmt == NULL)
	    call mfree (pntmt, TY_REAL)
	else
	    call imunmap (imnmt)
	call imunmap (imnout)
	call imunmap (imnin)
end
