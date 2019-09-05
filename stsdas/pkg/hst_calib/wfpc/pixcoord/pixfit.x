include "pixpos.h"
define	INV_TOL		1.0e-3

# Copyright restrictions apply - see stsdas$copyright.stsdas 
#
# The procedures in this file implement a least squares fit between the star
# positions as read from the star catalog (xold,yold) and the star positions
# as indicated by the user with the image cursor (xnew,ynew). By computing
# the least squares fit for the stars marked by the user, the positions of 
# all the stars in the catalog can be updated. The fit is computed in the
# (l,m) frame. This frame has its axes parallel to the world coordinate system
# axes (usually ra and dec) and has its origin at the reference point of the
# image (usually the point of tangency). The fit is given by the two equations
# below. All units are in pixels.
#
# (xnew - xold) = a1 + a2*xold + a3*yold + a4*xold^2 + a5*xold*yold + a6*yold^2
# (ynew - yold) = b1 + b2*yold + b3*xold + b4*yold^2 + b5*xold*yold + b6*xold^2
#
# B.Simon	17-Jul-90	Original

# APPLY_FIT -- Apply the least squares solution to compute the fitted position

procedure apply_fit (frm, maxsol, solution, xold, yold, xfit, yfit, npoint)

pointer	frm			# i: Coordinate frame structure
int	maxsol			# i: Length of solution vector
double	solution[maxsol,2]	# i: Solution vector
double	xold[ARB]		# i: Old x position in logical frame
double	yold[ARB]		# i: Old y position in logical frame
double	xfit[ARB]		# o: Fitted x position
double	yfit[ARB]		# o: Fitted y position
int	npoint			# i: Number of points
#--
int	ipoint
pointer	sp, poly

double	adotd()

begin
	call smark (sp)
	call salloc (poly, maxsol, TY_DOUBLE)

	call amovd (xold, xfit, npoint)
	call amovd (yold, yfit, npoint)

	call pixtolm (frm, xfit, yfit, npoint)

	do ipoint = 1, npoint {
	    call coef_fit (xfit[ipoint], yfit[ipoint], Memd[poly], maxsol)
	    xfit[ipoint] = xfit[ipoint] + 
			   adotd (Memd[poly], solution[1,1], maxsol)

	    call coef_fit (yfit[ipoint], xfit[ipoint], Memd[poly], maxsol)
	    yfit[ipoint] = yfit[ipoint] +
			   adotd (Memd[poly], solution[1,2], maxsol)
	}

	call lmtopix (frm, xfit, yfit, npoint)
	call sfree (sp)

end

# COEF_FIT -- Compute the coefficients of the polynomial equation 

procedure coef_fit (apos, bpos, poly, npoly)

double	apos		# i: Primary coordinate
double	bpos		# i: Secondary coordinate
double	poly[ARB]	# o: Polynomial coefficients
int	npoly		# i: Number of polynomial coefficients
#--
int	iorder, jorder, ipoly

begin
	iorder = 1
	jorder = 1

	poly[1] = 1.0
	do ipoly = 2, npoly {
	    if (jorder > iorder) {
		jorder = 1
		iorder = iorder + 1
		poly[ipoly] = poly[ipoly-iorder] * bpos
	    } else {
		jorder = jorder + 1
		poly[ipoly] = poly[ipoly-iorder] * apos
	    }
	}

end

# COORD_FIT -- Find the ra and dec for a point on the plate

procedure coord_fit (pos, frm, maxsol, solution, xold, yold, ra, dec)

pointer	pos			# i: Position descriptor
pointer	frm			# i: Coordinate frame structure
int	maxsol			# i: Length of solution vector
double	solution[maxsol,2]	# i: Solution vector
real	xold			# i: Old x position in logical frame
real	yold			# i: Old y position in logical frame
double	ra			# o: Right ascension of point
double	dec			# o: Declination of point
#--
include	"pixfit.com"

int	xlen, ylen
real	xinv, yinv

real	brent()

real    invert_fit() # Compute the inverse of the solution vector
extern	invert_fit

begin
	# Initialize common block variables

	sv_frm = frm
	sv_maxsol = maxsol
	sv_point[1] = xold
	sv_point[2] = yold
	call amovd (solution, sv_soln, 2*maxsol)

	# Convert (xold,yold) to inverse of fitted position (xiv, yinv)

	call rdipar_pos (pos, "xlen", xlen)
	call rdipar_pos (pos, "ylen", ylen)

	sv_axis = 1
	xinv = brent (invert_fit, 0.0, real(xlen), INV_TOL) 

	sv_axis = 2
	yinv = brent (invert_fit, 0.0, real(ylen), INV_TOL)

	# Convert inverse position to ra and dec

	ra = xinv
	dec = yinv
	call pixtoworld (frm, ra, dec, 1)

end

# INVERT_FIT -- Compute the inverse of the solution vector

real procedure invert_fit (point)

real	point		# i: The previous estimate of the fitted point
#--
include	"pixfit.com"

double	oldpnt[2], fitpnt[2]

begin

	oldpnt[1] = sv_point[1]
	oldpnt[2] = sv_point[2]
	oldpnt[sv_axis] = point

	call apply_fit (sv_frm, sv_maxsol, sv_soln, oldpnt[1], oldpnt[2],
			fitpnt[1], fitpnt[2], 1)

	return (fitpnt[sv_axis] - sv_point[sv_axis])
end

# LIST_FIT -- Print the results of the least squares fit

procedure list_fit (pos, frm, maxsol, solution)

pointer	pos			# i: Star position descriptor
pointer	frm			# i: Coordinate frame descriptor
int	maxsol			# i: Length of solution vector
double	solution[maxsol,2]	# i: Solution vector
#--
int	isol, istar, maxstar, nstar
pointer	sp, xold, yold, xnew, ynew, name

string	badcolumn  "Could not read column from pos descriptor (%s)"
string	soltitle   "   L solution%30t   M Solution\n"
string  solformat  "%15.7g%30t%15.7g\n"
string	postitle   "\n%10tXOLD%25tYOLD%40tXNEW%55tYNEW%70tNAME\n"
string	posformat  "%5d%15.7g%15.7g%15.7g%15.7g%4w%-s\n"

int	rddcol_pos(), rdtcol_pos()

begin
	call rdipar_pos (pos, "nstar", maxstar)

	call smark (sp)
	call salloc (xold, maxstar, TY_DOUBLE)
	call salloc (yold, maxstar, TY_DOUBLE)
	call salloc (xnew, maxstar, TY_DOUBLE)
	call salloc (ynew, maxstar, TY_DOUBLE)
	call salloc (name, maxstar, TY_INT)

	# Get star positions 

	if (rddcol_pos (pos, "xold", FLAG_IN, nstar, 
			Memd[xold], maxstar) == ERR)
	    call pixerror (badcolumn, "xold")

	if (rddcol_pos (pos, "yold", FLAG_IN, nstar, 
			Memd[yold], maxstar) == ERR)
	    call pixerror (badcolumn, "yold")

	if (rddcol_pos (pos, "xnew", FLAG_IN, nstar, 
			Memd[xnew], maxstar) == ERR)
	    call pixerror (badcolumn, "xnew")

	if (rddcol_pos (pos, "ynew", FLAG_IN, nstar, 
			Memd[ynew], maxstar) == ERR)
	    call pixerror (badcolumn, "ynew")

	if (rdtcol_pos (pos, "name", FLAG_IN, nstar, 
			Memi[name], maxstar) == ERR)
	    call pixerror (badcolumn, "name")

	# Put new star positions in lm frame

	call pixtolm (frm, Memd[xold], Memd[yold], nstar)
	call pixtolm (frm, Memd[xnew], Memd[ynew], nstar)

	call asubd (Memd[xnew], Memd[xold], Memd[xnew], nstar)
	call asubd (Memd[ynew], Memd[yold], Memd[ynew], nstar)

	# Print solution vector

	call fprintf (STDERR, soltitle)
	do isol = 1, maxsol {
	    call fprintf (STDERR, solformat)
	    call pargd (solution[isol,1])
	    call pargd (solution[isol,2])
	}

	# Print star coordinates

	call fprintf (STDERR, postitle)
	do istar = 1, nstar {
	    call fprintf (STDERR, posformat)
	    call pargi (istar)
	    call pargd (Memd[xold+istar-1])
	    call pargd (Memd[yold+istar-1])
	    call pargd (Memd[xnew+istar-1])
	    call pargd (Memd[ynew+istar-1])
	    call pargstr (Memc[Memi[name+istar-1]])
	}

	call sfree (sp)
end

# NORMEQ_FIT -- Set up the normal equations

procedure normeq_fit (anew, aold, bold, nstar, nterm, lhs, rhs)

double	anew[ARB]	# i: New position on principal axis
double	aold[ARB]	# i: Old position on principal axis
double	bold[ARB]	# i: Old position on secondary axis
int	nstar		# i: Number of stars
int	nterm		# i: Number of terms in solution
double	lhs[ARB]	# o: Left hand side of normal equation (matrix)
double	rhs[ARB]	# o: Right hand side of normal equation (vector)
#--
int	nmat, istar, i, j
pointer	sp, term

begin
	# Allocate work array

	call smark (sp)
	call salloc (term, nterm, TY_DOUBLE)

	# Compute size of output arrays and initialize them

	nmat = (nterm * (nterm + 1)) / 2

	call aclrd (lhs, nmat)
	call aclrd (rhs, nterm)

	do istar = 1, nstar {

	    # Compute the coefficient vector

	    call coef_fit (aold[istar], bold[istar], Memd[term], nterm)

	    # Add the vector into the normal equations

	    do i = 1, nterm {
		do j = i, nterm {
		    lhs[j*(j-1)/2+i] = lhs[j*(j-1)/2+i] + 
				       Memd[term+i-1] * Memd[term+j-1]
		 }
		 rhs[i] = rhs[i] + Memd[term+i-1] * anew[istar]
	    }
	}
	call sfree (sp)
end

# SOLVE_FIT -- Compute the least squares solution for the star positions

procedure solve_fit (pos, frm, nterm, maxsol, solution, done)

pointer	pos			# i: Star position descriptor
pointer	frm			# i: Coordinate frame structure
int	nterm			# i: Number of terms in least squares solution
int	maxsol			# i: Maximum length of solution vector
double	solution[maxsol,2]	# o: Solution vector
bool	done			# o: Was least squares solution performed?
#--
int	maxstar, nstar, nvec, nmat
pointer	sp, lhs, rhs, xold, yold, xnew, ynew

string	badcolumn  "Could not read column from pos descriptor (%s)"
string	nostars	   "Not enough stars to perform least squares solution"

int	rddcol_pos()

begin
	# Compute number of stars to fit

	call rdipar_pos (pos, "nstar", maxstar)

	# Allocate memory for star positions

	call smark (sp)
	call salloc (xold, maxstar, TY_DOUBLE)
	call salloc (yold, maxstar, TY_DOUBLE)
	call salloc (xnew, maxstar, TY_DOUBLE)
	call salloc (ynew, maxstar, TY_DOUBLE)

	# Read star positions from pos structure

	if (rddcol_pos (pos, "xold", FLAG_IN, nstar, 
			Memd[xold], maxstar) == ERR)
	    call pixerror (badcolumn, "xold")

	if (rddcol_pos (pos, "yold", FLAG_IN, nstar, 
			Memd[yold], maxstar) == ERR)
	    call pixerror (badcolumn, "yold")

	if (rddcol_pos (pos, "xnew", FLAG_IN, nstar, 
			Memd[xnew], maxstar) == ERR)
	    call pixerror (badcolumn, "xnew")

	if (rddcol_pos (pos, "ynew", FLAG_IN, nstar, 
			Memd[ynew], maxstar) == ERR)
	    call pixerror (badcolumn, "ynew")

	done = nstar >= nterm
	if (done) {
	    call aclrd (solution, 2*maxsol)	# Initialize solution array
	} else {
	    call pixmessage (nostars, "")
	    call sfree (sp)
	    return
	}

	# Rotate star positions to (l,m) frame and
	# compute difference between new and old positions

	call pixtolm (frm, Memd[xold], Memd[yold], nstar)
	call pixtolm (frm, Memd[xnew], Memd[ynew], nstar)

	call asubd (Memd[xnew], Memd[xold], Memd[xnew], nstar)
	call asubd (Memd[ynew], Memd[yold], Memd[ynew], nstar)

	# Allocate normal equation vector and matrix

	nvec = min (nterm, maxsol)
	nmat = (nvec * (nvec + 1)) / 2

	call salloc (lhs, nmat, TY_DOUBLE)
	call salloc (rhs, nvec, TY_DOUBLE)

	# Perform least squares solution

	call normeq_fit (Memd[xnew], Memd[xold], Memd[yold], nstar, nvec, 
			 Memd[lhs], Memd[rhs])
	call cholesky (nvec, Memd[lhs], Memd[rhs], solution[1,1])

	call normeq_fit (Memd[ynew], Memd[yold], Memd[xold], nstar, nvec, 
			 Memd[lhs], Memd[rhs])
	call cholesky (nvec, Memd[lhs], Memd[rhs], solution[1,2])

	call sfree (sp)

end

# STATS_FIT -- Print statistics on least squares fit

procedure stats_fit (pos, frm, maxsol, solution)

pointer	pos			# i: Star position descriptor
pointer	frm			# i: Coordinate frame descriptor
int	maxsol			# i: Length of solution vector
double	solution[maxsol,2]	# i: Solution vector
#--
double	resid, sumresid, maxresid, meanresid
int	istar, jstar, mstar, nstar, maxstar, junk
pointer	sp, xold, yold, xnew, ynew, xfit, yfit, index, name

string	badcolumn  "Could not read column from pos descriptor (%s)"
string	fmt_mean   "The average residual of the fit is %0.1f pixels\n"
string	fmt_max    "The largest residual is %0.1f pixels,\n"
string	fmt_name   "From star number %d, named %s\n"

int	rddcol_pos(), rdindex_pos(), rdtval_pos()

begin
	call rdipar_pos (pos, "nstar", maxstar)

	call smark (sp)
	call salloc (xold, maxstar, TY_DOUBLE)
	call salloc (yold, maxstar, TY_DOUBLE)
	call salloc (xnew, maxstar, TY_DOUBLE)
	call salloc (ynew, maxstar, TY_DOUBLE)
	call salloc (xfit, maxstar, TY_DOUBLE)
	call salloc (yfit, maxstar, TY_DOUBLE)
	call salloc (index, maxstar, TY_INT)
	call salloc (name, SZ_FNAME, TY_CHAR)

	# Get star catalog positions 

	if (rddcol_pos (pos, "xold", FLAG_IN, nstar, 
			Memd[xold], maxstar) == ERR)
	    call pixerror (badcolumn, "xold")

	if (rddcol_pos (pos, "yold", FLAG_IN, nstar, 
			Memd[yold], maxstar) == ERR)
	    call pixerror (badcolumn, "yold")

	if (rddcol_pos (pos, "xnew", FLAG_IN, nstar, 
			Memd[xnew], maxstar) == ERR)
	    call pixerror (badcolumn, "xnew")

	if (rddcol_pos (pos, "ynew", FLAG_IN, nstar, 
			Memd[ynew], maxstar) == ERR)
	    call pixerror (badcolumn, "ynew")

	if (rdindex_pos (pos, FLAG_IN, nstar, Memi[index], maxstar) == ERR)
	    call pixerror (badcolumn, "index")

	# Compute fitted positions

	call apply_fit (frm, maxsol, solution, Memd[xold], Memd[yold], 
			Memd[xfit], Memd[yfit], nstar)

	# Compute mean and maximum resdidual

	jstar = 0
	mstar = 0
	sumresid = 0.0
	maxresid = -1.0
	meanresid = 0.0

	do istar = 1, nstar {
	    mstar = mstar + 1
	    resid = sqrt ((Memd[xfit+istar-1] - Memd[xnew+istar-1]) ** 2 +
			  (Memd[yfit+istar-1] - Memd[ynew+istar-1]) ** 2  )
	    sumresid = sumresid + resid
	    if (resid > maxresid) {
		jstar = Memi[index+istar-1]
		maxresid = resid
	    }
	}

	if (mstar > 0)
	    meanresid = sumresid / mstar

	call fprintf (STDERR, fmt_mean)
	call pargd (meanresid)

	if (jstar > 0) {
	    junk = rdtval_pos (pos, "name", FLAG_NULL, jstar, 
			       Memc[name], SZ_FNAME)

	    call fprintf (STDERR, fmt_max)
	    call pargd (maxresid)

	    call fprintf (STDERR, fmt_name)
	    call pargi (jstar)
	    call pargstr (Memc[name])
	}

	call putci (STDERR, '\n')
	call flush (STDERR)

end
