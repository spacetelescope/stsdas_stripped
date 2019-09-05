include <imhdr.h>
include <math/iminterp.h>
include	"qmosaic.h"

#  qmosaic_do -- put each frame into the proper quardrant
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  31-Mar-1994  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure qmosaic_do (ipin, fin, grp, fout, outbuf, interp)

pointer	ipin
char	fin[SZ_FNAME]
int	grp
char	fout[SZ_FNAME]
real	outbuf[SZ_OUT, SZ_OUT]
char	interp[SZ_LINE]

int	iccd
int	i, j
int	dim_x, dim_y, size_x, size_y, ox, oy, x0, y0
int	sz_x, sz_y
pointer	pic, idx, tmp
char	instr[SZ_LINE]

int	imgeti()
pointer	imgs2r()
bool	streq(), strne()
#==============================================================================
begin

	# read the detector number and instrument name
	iccd = imgeti (ipin, "DETECTOR")
	call imgstr (ipin, "INSTRUME", instr, SZ_LINE)

	# define shadow areas for each detector
	if (streq(instr, "WFPC")) {
	    if (iccd == 1) {
		x0 = 19
		y0 = 29
	    } else if (iccd == 2) {
		x0 = 13
		y0 = 25
	    } else if (iccd == 3) {
		x0 = 21
		y0 = 26
	    } else if (iccd == 4) {
		x0 = 18
		y0 = 28
	    } else if (iccd == 5) {
		x0 = 32
		y0 = 30
	    } else if (iccd == 6) {
		x0 = 20
		y0 = 26
	    } else if (iccd == 7) {
		x0 = 24
		y0 = 29
	    } else if (iccd == 8) {
		x0 = 16
		y0 = 33
	    }
	} else if (streq(instr, "WFPC2")) {
	    if (iccd == 1) {
		x0 = 44
		y0 = 53
	    } else if (iccd == 2) {
		x0 = 43
		y0 = 24
	    } else if (iccd == 3) {
		x0 = 29
		y0 = 46
	    } else if (iccd == 4) {
		x0 = 43
		y0 = 41
	    }
	}

	dim_x = IM_LEN (ipin, 1)
	dim_y = IM_LEN (ipin, 2)

	# read the input data
	pic = imgs2r(ipin, 1, dim_x, 1, dim_y)

	size_x = min (dim_x, DIM_X+x0-1)
	size_y = min (dim_y, DIM_Y+y0-1)

	# group 2 is in the 3rd quardrant
	if (iccd == 2 || iccd == 6) {
	    call prmap (fin, grp, x0, size_x, y0, size_y, fout, DIM_X, 
			DIM_X+x0-size_x, DIM_Y, DIM_Y+y0-size_y)
	    do j = y0, size_y {
		oy = DIM_Y+y0-j
		idx = pic+(j-1)*dim_x-1
		do i = x0, size_x {
		    ox = DIM_X+x0-i
		    outbuf[ox, oy] = Memr[idx+i]
		}
	    }

	# group 3 is in the 4th quardrant
	} else if (iccd == 3 || iccd == 7) {
	    call prmap (fin, grp, x0, size_x, y0, size_y, fout, DIM_X+1, 
			DIM_X+1+size_y-y0, DIM_Y, DIM_Y+x0-size_x)
	    do j = y0, size_y {
		ox = DIM_X+1+j-y0
		idx = pic+(j-1)*dim_x-1
		do i = x0, size_x {
		    oy = DIM_Y+x0-i
		    outbuf[ox, oy] = Memr[idx+i]
		}
	    }

	# group 4 is in the 1st quardrant
	} else if (iccd == 4 || iccd == 8) {
	    call prmap (fin, grp, x0, size_x, y0, size_y, fout, DIM_X+1, 
			DIM_X+1+size_x-x0, DIM_Y+1, DIM_Y+1+size_y-y0)
	    do j = y0, size_y {
		oy = DIM_Y+1+j-y0
		idx = pic+(j-1)*dim_x-1
		do i = x0, size_x {
		    ox = DIM_X+1+i-x0
		    outbuf[ox, oy] = Memr[idx+i]
		}
	    }

	# group 1 is in the 2nd quardrant
	} else if (iccd == 1 || iccd == 5) {
	    if (streq(instr, "WFPC2") && strne(interp, "none")) {
		sz_x = int(real(size_x-x0+1)/XSCALE)
		sz_y = int(real(size_y-y0+1)/YSCALE)
	    	call prmap (fin, grp, x0, size_x, y0, size_y, fout, DIM_X, 
			DIM_X-sz_y+1, DIM_Y+1, DIM_Y+sz_x)
		call malloc (tmp, SZ_PC*SZ_PC, TY_REAL)
		call u_shrink (pic, dim_x, dim_y, x0, y0, Memr[tmp], interp)
	    	do j = 1, SZ_PC {
		    ox = DIM_X-j+1
		    idx = tmp+(j-1)*SZ_PC-1
		    do i = 1, SZ_PC {
		    	oy = DIM_Y+i
		    	outbuf[ox, oy] = Memr[idx+i]
		    }
		}
		call mfree (tmp, TY_REAL)
	    } else {
	    	call prmap (fin, grp, x0, size_x, y0, size_y, fout, DIM_X, 
			DIM_X-size_y+y0, DIM_Y+1, DIM_Y+1+size_x-x0)
	    	do j = y0, size_y {
		    ox = DIM_X-j+y0
		    idx = pic+(j-1)*dim_x-1
		    do i = x0, size_x {
		    	oy = DIM_Y+1+i-x0
		    	outbuf[ox, oy] = Memr[idx+i]
		    }
		}
	    }
	}
end

#  prmap -- print out the mapping message
#
#------------------------------------------------------------------------------

procedure prmap (fin, grp, inx1, inx2, iny1, iny2, fout, outx1, outx2,
		outy1, outy2)	

char	fin[SZ_FNAME]
int	grp
int	inx1, inx2, iny1, iny2
char	fout[SZ_FNAME]
int	outx1, outx2, outy1, outy2
#==============================================================================
begin
	call printf ("Copy %s[%d][%d:%d,%d:%d] -> %s[%d:%d,%d:%d]\n")
	    call pargstr(fin)
	    call pargi(grp)
	    call pargi(inx1)
	    call pargi(inx2)
	    call pargi(iny1)
	    call pargi(iny2)
	    call pargstr(fout)
	    call pargi(outx1)
	    call pargi(outx2)
	    call pargi(outy1)
	    call pargi(outy2)
	call flush (STDOUT)
end

#  u_shrink -- Shrink the PC frame of WFPC2
#
#------------------------------------------------------------------------------

procedure u_shrink (pic, dim_x, dim_y, x0, y0, out, interp)

pointer	pic
int	dim_x, dim_y
int	x0, y0
real	out[SZ_PC, SZ_PC]
char	interp[SZ_LINE]

pointer	msi, x, y
int	sz_x, sz_y, size_x, size_y
int	itype
int	i, j

bool	streq()
#==============================================================================
begin
	size_x = min (dim_x, DIM_X+x0-1)
	size_y = min (dim_y, DIM_Y+y0-1)
	sz_x = int(real(size_x-x0+1)/XSCALE)
	sz_y = int(real(size_y-y0+1)/YSCALE)

	# reset the output buffer
	do j = 1, SZ_PC {
	    do i = 1, SZ_PC
		out[i,j] = 0.
	}

	if (streq(interp, "nearest")) itype = II_BINEAREST
	if (streq(interp, "linear")) itype = II_BILINEAR
	if (streq(interp, "poly3")) itype = II_BIPOLY3
	if (streq(interp, "poly5")) itype = II_BIPOLY5
	if (streq(interp, "spline3")) itype = II_BISPLINE3

	call msiinit (msi, itype)
	call msifit (msi, Memr[pic], dim_x, dim_y, dim_x)

	call malloc (x, sz_x, TY_REAL)
	call malloc (y, sz_x, TY_REAL)
	do i = 1, sz_x
	    Memr[x+i-1] = real(x0) + XSCALE * real (i-1)

	do j = 1, sz_y {
	    call amovkr (real(y0)+YSCALE*real(j-1), Memr[y], sz_x)
	    call msivector (msi, Memr[x], Memr[y], out[1,j], sz_x)
	}
	
	# normalize the flux
	call amulkr (out, XSCALE*YSCALE, out, SZ_PC*SZ_PC)
	call msifree (msi)
	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL)
end
