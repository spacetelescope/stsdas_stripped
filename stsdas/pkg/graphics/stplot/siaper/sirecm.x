include "siaper.h"

# si_rect_form - Get the vectors necessary for a rectangle.
#
# 15Mar91 - Finished code by Andrew Cseko.  Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure si_rect_form (siaf_tp, colptr, row, x, y, npts)

pointer siaf_tp         # I:  The SIAF table descriptor.
pointer colptr[N_COLS]  # I:  The pointers to columns in the SIAF file.
int     row             # I:  The row of the table that has the info.
pointer x, y            # O:  The vectors representing the circle.
int     npts            # O:  The number of vectors in the circle.

# Declarations
real maj_axis           # Length of the rectangle in the Y dimension.
real min_axis           # Length of the rectangle in the X dimension.
int	parity		# Parity.
real rot_angle          # Angle (degrees) to rotate the rectangle.
pointer tx, ty          # Temporary vectors.

begin

	# Get the SIAF info.
	call tbegtr (siaf_tp, colptr[MAJ_AXIS], row, maj_axis)
	call tbegtr (siaf_tp, colptr[MIN_AXIS], row, min_axis)
	call tbegtr (siaf_tp, colptr[ROT_ANGLE], row, rot_angle)
	call tbegti (siaf_tp, colptr[PARITY], row, parity)

	# Allocate the memory.
	npts = N_RECT
	call malloc (x, npts, TY_REAL)
	call malloc (y, npts, TY_REAL)
	call malloc (tx, npts, TY_REAL)
	call malloc (ty, npts, TY_REAL)

	# Setup the rectangle vectors
	ONER(tx,1) = -min_axis / 2.
	ONER(ty,1) = -maj_axis / 2.
	ONER(tx,2) = ONER(tx,1) + min_axis
	ONER(ty,2) = ONER(ty,1)
	ONER(tx,3) = ONER(tx,2)
	ONER(ty,3) = ONER(ty,2) + maj_axis
	ONER(tx,4) = ONER(tx,1)
	ONER(ty,4) = ONER(ty,3)
	ONER(tx,5) = ONER(tx,1)
	ONER(ty,5) = ONER(ty,1)

	# Now rotate the points through their orientation.
	call si_rot (ONER(tx,1), ONER(ty,1), npts, rot_angle, parity,
		     ONER(x,1), ONER(y,1))

	# Free the local vectors.
	call mfree (tx, TY_REAL)
	call mfree (ty, TY_REAL)

end
#---------------------------------------------------------------------------
# End of si_rect_form
#---------------------------------------------------------------------------
