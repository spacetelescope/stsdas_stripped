include "siaper.h"

# si_pickle_form - Get the vectors necessary for a pickle object.
#
# History
#   15Mar91 - Finished code by Andrew Cseko. Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure si_pickle_form (siaf_tp, colptr, row, x, y, npts)

pointer siaf_tp         # I:  The SIAF table descriptor.
pointer colptr[N_COLS]  # I:  The pointers to columns in the SIAF file.
int     row             # I:  Row of the SIAF table that has the info.
pointer x, y            # O:  The vectors representing the circle.
int     npts            # O:  The number of vectors in the circle.

# Declarations
pointer tx, ty          # Temporary graphics vectors.

real in_ang_ext         # Size of the inner arc.
real in_radius          # Inner radius of the pickle.
real in_rot_ang         # Angle relative to V3 of the center of the
                        # inner arc of the pickle.
real out_ang_ext        # Size of the outer arc.
real out_radius         # Outer radius of the pickle.
real out_rot_ang        # Angle relative to V3 of the center of the 
                        # outer arc of the pickle.

begin

	# Get the info from the table.
	call tbegtr (siaf_tp, colptr[MIN_AXIS], row, in_radius)
	call tbegtr (siaf_tp, colptr[MAJ_AXIS], row, out_radius)
	call tbegtr (siaf_tp, colptr[OUT_ROT_ANG], row, out_rot_ang)
	call tbegtr (siaf_tp, colptr[OUT_ANG_EXT], row, out_ang_ext)
	call tbegtr (siaf_tp, colptr[IN_ROT_ANG], row, in_rot_ang)
	call tbegtr (siaf_tp, colptr[IN_ANG_EXT], row, in_ang_ext)

	# Allocate the vectors.
	npts = (2 * N_ARC_SEGMENTS) + 1
	call malloc (x, npts, TY_REAL)
	call malloc (y, npts, TY_REAL)
	call malloc (tx, npts, TY_REAL)
	call malloc (ty, npts, TY_REAL)

	# Find the pickle arcs.
	call si_arc_form (out_radius, out_rot_ang - out_ang_ext,
			  2. * out_ang_ext, ONER(tx,1), ONER(ty,1), N_ARC_SEGMENTS)
	call si_arc_form (in_radius, in_rot_ang - in_ang_ext,
			  2. * in_ang_ext, ONER(tx,N_ARC_SEGMENTS+1), 
			  ONER(ty,N_ARC_SEGMENTS+1), N_ARC_SEGMENTS)

	# Reverse the order of the vectors for the second arc.
	call si_reverse (ONER(tx,N_ARC_SEGMENTS+1), ONER(ty,N_ARC_SEGMENTS+1), 
			 N_ARC_SEGMENTS)

	# Set the last vector to be back to the beginning of the first arc.
	ONER(tx,npts) = ONER(tx,1)
	ONER(ty,npts) = ONER(ty,1)

	# Copy them out.
	call amovr (ONER(tx,1), ONER(x,1), npts)
	call amovr (ONER(ty,1), ONER(y,1), npts)

	# Free temporary vectors.
	call mfree (tx, TY_REAL)
	call mfree (ty, TY_REAL)

end
#---------------------------------------------------------------------------
# End of si_pickle_form
#---------------------------------------------------------------------------
