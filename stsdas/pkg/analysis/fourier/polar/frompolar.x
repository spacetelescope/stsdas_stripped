include <imhdr.h>

# t_frompolar -- convert from amplitude and phase to real & imaginary parts
#
# Phil Hodge, 19-July-1990  Task created.

procedure t_frompolar()

char	inp_a[SZ_FNAME], inp_p[SZ_FNAME]
char	outp[SZ_FNAME]
#--
char	outpr[SZ_FNAME], outpi[SZ_FNAME]
pointer im_a, im_p, oimr, oimi
pointer x_a, x_p, oxr, oxi
int	k
int	v_a[IM_MAXDIM], v_p[IM_MAXDIM], ovr[IM_MAXDIM], ovi[IM_MAXDIM]
int	stat
pointer immap(), imgnlr(), impnlr()

begin
	call clgstr ("ampl", inp_a, SZ_FNAME)
	call clgstr ("phase", inp_p, SZ_FNAME)
	call clgstr ("output", outp, SZ_FNAME)

	im_a = immap (inp_a, READ_ONLY, NULL)
	im_p = immap (inp_p, READ_ONLY, NULL)

	call ft_fname (outp, "r", outpr, SZ_FNAME)
	call ft_fname (outp, "i", outpi, SZ_FNAME)
	oimr = immap (outpr, NEW_COPY, im_a)
	oimi = immap (outpi, NEW_COPY, im_a)

	do k = 1, IM_MAXDIM {
	    v_a[k] = 1
	    v_p[k] = 1
	    ovr[k] = 1
	    ovi[k] = 1
	}

	while (imgnlr (im_a, x_a, v_a) != EOF) {
	    stat = imgnlr (im_p, x_p, v_p)
	    stat = impnlr (oimr, oxr, ovr)
	    stat = impnlr (oimi, oxi, ovi)

	    do k = 0, IM_LEN(1,im_a)-1 {
		Memr[oxr+k] = Memr[x_a+k] * cos (Memr[x_p+k])
		Memr[oxi+k] = Memr[x_a+k] * sin (Memr[x_p+k])
	    }
	}

	call imunmap (oimi)
	call imunmap (oimr)
	call imunmap (im_p)
	call imunmap (im_a)
end
