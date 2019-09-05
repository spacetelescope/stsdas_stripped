include	"pixframe.h"

define	NDIM		2
define	NFRAME		NDIM*(NDIM+1)

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# PIXFRAME -- Coordinate tranformations between reference frames
#
# B.Simon	17-Jul-90	Original

# CLS_FRAME -- Free the frame data structure

procedure cls_frame (frm, mw)

pointer	frm		# i: Frame structure
pointer	mw		# o: MWCS structure
#--

begin
	mw = FRM_MWCS(frm)

	call mfree (FRM_PX2LM(frm), TY_DOUBLE)
	call mfree (FRM_LM2PX(frm), TY_DOUBLE)
	call mfree (frm, TY_STRUCT)

end

# INIT_FRAME -- Initialize the frame data structure

procedure init_frame (mw, frm)

pointer	mw		# i: MWCS structure
pointer	frm		# o: Frame structure
#--
double	ltm[NDIM,NDIM], ltv[NDIM]
double	rphys[NDIM], wphys[NDIM], cdphys[NDIM,NDIM]
double	rlog[NDIM], cdlog[NDIM,NDIM], cdinv[NDIM,NDIM]
int	i, j
pointer	ct, px2lm, lm2px

pointer	mw_sctran()

begin
	call malloc (frm, LEN_FRAME, TY_STRUCT)

	FRM_MWCS(frm) = mw
	call malloc (FRM_PX2LM(frm), NFRAME, TY_DOUBLE)
	call malloc (FRM_LM2PX(frm), NFRAME, TY_DOUBLE)

	call mw_gltermd (mw, ltm, ltv, NDIM)
	call mw_gwtermd (mw, rphys, wphys, cdphys, NDIM)

	ct = mw_sctran (mw, "physical", "logical", 3)
	call mw_ctrand (ct, rphys, rlog, NDIM)
	call mw_ctfree (ct)

	call mw_mmuld (ltm, cdphys, cdlog, NDIM)
	call mw_invertd (cdlog, cdinv, NDIM)

	px2lm = FRM_PX2LM(frm)
	lm2px = FRM_LM2PX(frm)

	do i = 1, NDIM {
	    do j = 1, NDIM {
		Memd[px2lm] = cdlog[i,j]
		Memd[lm2px] = cdinv[i,j]
		px2lm = px2lm + 1
		lm2px = lm2px + 1
	    }
	    Memd[px2lm] = - rlog[i]
	    Memd[lm2px] = rlog[i]
	    px2lm = px2lm + 1
	    lm2px = lm2px + 1
	}

end

# LMTOPIX -- Tranform points from (l,m) to logical pixel frame

procedure lmtopix (frm, xpos, ypos, npoint)

pointer	frm		#  i: Frame structure
double	xpos[ARB]	# io: First coordinate position
double	ypos[ARB]	# io: Second coordinate position
int	npoint		#  i: Number of points
#--
double	xtemp, ytemp
int	ipoint
pointer	lm2px

begin
	lm2px = FRM_LM2PX(frm)

	do ipoint = 1, npoint {
	    xtemp = Memd[lm2px] * xpos[ipoint] + 
		    Memd[lm2px+1] * ypos[ipoint] + Memd[lm2px+2]
	    ytemp = Memd[lm2px+3] * xpos[ipoint] +
		    Memd[lm2px+4] * ypos[ipoint] + Memd[lm2px+5]
	    xpos[ipoint] = xtemp
	    ypos[ipoint] = ytemp
	}
end

# PIXTOLM -- Transform points from pixel to (l,m) frame

procedure pixtolm (frm, xpos, ypos, npoint)

pointer	frm		#  i: Frame structure
double	xpos[ARB]	# io: First coordinate position
double	ypos[ARB]	# io: Second coordinate position
int	npoint		#  i: Number of points
#--
double	xtemp, ytemp
int	ipoint
pointer	px2lm

begin
	px2lm = FRM_PX2LM(frm)

	do ipoint = 1, npoint {
	    xtemp = Memd[px2lm] * (xpos[ipoint] + Memd[px2lm+2]) + 
		    Memd[px2lm+1] * (ypos[ipoint] + Memd[px2lm+5])
	    ytemp = Memd[px2lm+3] * (xpos[ipoint] + Memd[px2lm+2]) + 
		    Memd[px2lm+4] * (ypos[ipoint] + Memd[px2lm+5])
	    xpos[ipoint] = xtemp
	    ypos[ipoint] = ytemp
	}
end

# PIXTOWORLD -- Transform points from pixel to world coordinate frame

procedure pixtoworld (frm, xpos, ypos, npoint)

pointer	frm		#  i: Frame structure
double	xpos[ARB]	# io: First coordinate position
double	ypos[ARB]	# io: Second coordinate position
int	npoint		#  i: Number of points
#--
double	xtemp, ytemp
int	ipoint
pointer	ct, mw

pointer	mw_sctran()

begin
	mw = FRM_MWCS(frm)
	ct = mw_sctran (mw, "logical", "world", 3)
	do ipoint = 1, npoint {
	    call mw_c2trand (ct, xpos[ipoint], ypos[ipoint], xtemp, ytemp)
	    xpos[ipoint] = xtemp
	    ypos[ipoint] = ytemp
	}
	call mw_ctfree (ct)

end

# WORLDTOPIX -- Transform points from world to pixel coordinate frame

procedure worldtopix (frm, xpos, ypos, npoint)

pointer	frm		#  i: Frame structure
double	xpos[ARB]	# io: First coordinate position
double	ypos[ARB]	# io: Second coordinate position
int	npoint		#  i: Number of points
#--
double	xtemp, ytemp
int	ipoint
pointer	ct, mw

pointer	mw_sctran()

begin
	mw = FRM_MWCS(frm)
	ct = mw_sctran (mw, "world", "logical", 3)
	do ipoint = 1, npoint {
	    call mw_c2trand (ct, xpos[ipoint], ypos[ipoint], xtemp, ytemp)
	    xpos[ipoint] = xtemp
	    ypos[ipoint] = ytemp
	}
	call mw_ctfree (ct)

end
