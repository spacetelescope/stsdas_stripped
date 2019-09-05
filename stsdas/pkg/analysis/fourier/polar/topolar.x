include <imhdr.h>

# t_topolar -- convert from real & imaginary parts to amplitude & phase
#
# Phil Hodge, 19-July-1990  Task created.
# Phil Hodge,  9-Apr-1991  Check for zero arguments in atan2 function call.

procedure t_topolar()

char	input[SZ_FNAME]
char	output_a[SZ_FNAME], output_p[SZ_FNAME]
#--
char	inputr[SZ_FNAME], inputi[SZ_FNAME]
pointer imr, imi, oim_a, oim_p		# pointers to imhdr struct
pointer xr, xi, ox_a, ox_p		# pointers to image data (lines)
real	ampl				# amplitude at a pixel
int	k
int	vr[IM_MAXDIM], vi[IM_MAXDIM], ov_a[IM_MAXDIM], ov_p[IM_MAXDIM]
int	stat
pointer immap(), imgnlr(), impnlr()

begin
	call clgstr ("input", input, SZ_FNAME)
	call clgstr ("ampl", output_a, SZ_FNAME)
	call clgstr ("phase", output_p, SZ_FNAME)

	call ft_fname (input, "r", inputr, SZ_FNAME)
	call ft_fname (input, "i", inputi, SZ_FNAME)
	imr = immap (inputr, READ_ONLY, NULL)
	imi = immap (inputi, READ_ONLY, NULL)

	oim_a = immap (output_a, NEW_COPY, imr)
	oim_p = immap (output_p, NEW_COPY, imr)

	# Initialize for getting/putting consecutive lines.
	do k = 1, IM_MAXDIM {
	    vr[k] = 1
	    vi[k] = 1
	    ov_a[k] = 1
	    ov_p[k] = 1
	}

	while (imgnlr (imr, xr, vr) != EOF) {
	    stat = imgnlr (imi, xi, vi)
	    stat = impnlr (oim_a, ox_a, ov_a)
	    stat = impnlr (oim_p, ox_p, ov_p)

	    do k = 0, IM_LEN(1,imr)-1 {
		ampl = sqrt (Memr[xr+k]**2 + Memr[xi+k]**2)	# amplitude
		Memr[ox_a+k] = ampl
		if (ampl > 0.)
		    Memr[ox_p+k] = atan2 (Memr[xi+k], Memr[xr+k])	# phase
		else
		    Memr[ox_p+k] = 0.
	    }
	}

	call imunmap (oim_p)
	call imunmap (oim_a)
	call imunmap (imi)
	call imunmap (imr)
end
