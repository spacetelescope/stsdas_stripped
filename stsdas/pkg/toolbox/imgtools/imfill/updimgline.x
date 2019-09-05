# UPD_IMGLINE -- Update the image line

procedure upd_imgline (img, code, value, imglen, imgtype, exptype)

pointer	img		# i: Image descriptor
pointer	code		# i: Pseudocode descriptor
double	value		# i: Fill value
int	imglen		# i: Length of an image line
int	imgtype		# i: Data type of image
int	exptype		# i: Data type of expression
#--
int	i
pointer	mline, iline, oline, tline

pointer	rd_fullimg(), wrt_fullimg()

begin 
	call malloc (mline, imglen, TY_INT)
	iline = rd_fullimg (img)

	call bup_fullimg (img)
	oline = wrt_fullimg (img)

	if (exptype == TY_INT) {
	    call vex_copyi (code, 0, Memi[mline], imglen)

	} else {
	    call malloc (tline, imglen, TY_REAL)

	    call vex_copyr (code, 0.0, Memr[tline], imglen)
	    call abnekr (Memr[tline], 0.0, Memi[mline], imglen)

	    call mfree (tline, TY_REAL)
	}

	switch (imgtype) {
	case TY_SHORT, TY_INT, TY_LONG, TY_USHORT:
	    call amovi (Memi[iline], Memi[oline], imglen)
	    do i = 0, imglen-1 {
		if (Memi[mline+i] != 0)
		    Memi[oline+i] = value
	    }
	case TY_REAL:
	    call amovr (Memr[iline], Memr[oline], imglen)
	    do i = 0, imglen-1 {
		if (Memi[mline+i] != 0)
		    Memr[oline+i] = value
	    }
	case TY_DOUBLE:
	    call amovd (Memd[iline], Memd[oline], imglen)
	    do i = 0, imglen-1 {
		if (Memi[mline+i] != 0)
		    Memd[oline+i] = value
	    }
	default:
	    call amovd (Memd[iline], Memd[oline], imglen)
	    do i = 0, imglen-1 {
		if (Memi[mline+i] != 0)
		    Memd[oline+i] = value
	    }
	}

	call mfree (mline, TY_INT)
end
