include "siaper.h"

# si_circle_form - Produce the vectors necessary for a circle.
#
# History
#    15Mar91 - Modified code by Andrew Cseko. Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure si_circle_form (siaf_tp, colptr, row, x, y, npts)

pointer siaf_tp         # I:  The SIAF table descriptor.
pointer colptr[N_COLS]  # I:  The pointers to columns in the SIAF file.
int     row             # I:  The row of the table that has the info.
pointer x, y            # O:  The vectors representing the circle.
int     npts            # O:  The number of vectors in the circle.

# Declarations
real diameter           # Diameter of the circle.

begin

	# Get the necessary parameters.
	call tbegtr (siaf_tp, colptr[MAJ_AXIS], row, diameter)

	# Allocate the vector memory.
	npts = N_ARC_SEGMENTS
	call malloc (x, npts, TY_REAL)
	call malloc (y, npts, TY_REAL)

	# Produce the vectors that will define the circle around the
	# origin.
	call si_arc_form (diameter / 2., 0., 360., ONER(x,1), ONER(y,1), npts)

end
#---------------------------------------------------------------------------
# End of si_circle_form
#---------------------------------------------------------------------------
