include "siaper.h"

# si_quad_form - Get the vectors necessary for a quad object.
#
# 15Mar91 - Finished code by Andrew Cseko. Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure si_quad_form (siaf_tp, colptr, row, x, y, npts)

pointer siaf_tp         # I:  The SIAF table descriptor.
pointer colptr[N_COLS]  # I:  The pointers to columns in the SIAF file.
int     row             # I:  The row of the table that has the info.
pointer x, y            # O:  The vectors representing the circle.
int     npts            # O:  The number of vectors in the circle.

# Declarations
int	parity		# Parity.
pointer tx, ty          # Temporary vectors.

real rot_angle          # Angle (degrees) to rotate the quad.

begin
	npts = N_RECT
	call malloc (x, npts, TY_REAL)
	call malloc (y, npts, TY_REAL)
	call malloc (tx, npts, TY_REAL)
	call malloc (ty, npts, TY_REAL)

	# Get the SIAF info.
	call tbegtr (siaf_tp, colptr[ROT_ANGLE], row, rot_angle)
	call tbegtr (siaf_tp, colptr[VRT1_X], row, ONER(tx,1))
	call tbegtr (siaf_tp, colptr[VRT1_Y], row, ONER(ty,1))
	call tbegtr (siaf_tp, colptr[VRT2_X], row, ONER(tx,2))
	call tbegtr (siaf_tp, colptr[VRT2_Y], row, ONER(ty,2))
	call tbegtr (siaf_tp, colptr[VRT3_X], row, ONER(tx,3))
	call tbegtr (siaf_tp, colptr[VRT3_Y], row, ONER(ty,3))
	call tbegtr (siaf_tp, colptr[VRT4_X], row, ONER(tx,4))
	call tbegtr (siaf_tp, colptr[VRT4_Y], row, ONER(ty,4))
	ONER(tx,5) = ONER(tx,1)
	ONER(ty,5) = ONER(ty,1)

	call tbegti (siaf_tp, colptr[PARITY], row, parity)
	
	# Now rotate the points through their orientation.
	call si_rot (ONER(tx,1), ONER(ty,1), npts, rot_angle, parity,
			ONER(x,1), ONER(y,1))

	# Free the temporary pointers.
	call mfree (tx, TY_REAL)
	call mfree (ty, TY_REAL)

end
#---------------------------------------------------------------------------
# End of si_quad_form
#---------------------------------------------------------------------------
