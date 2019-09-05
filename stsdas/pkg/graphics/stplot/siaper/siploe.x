include "siaper.h"

#---------------------------------------------------------------------------
# si_plot_aperture - Plot the apertures.
#
# History
#  7Mar91 - Finish code begun by Andrew Cseko.
#           Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

procedure si_plot_aperture (aperture_name, siaf_tp, colptr, gp, ct, flip)

char    aperture_name[ARB]  # I: instrument aperture interested in 
pointer siaf_tp             # I: pointer to SIAF data table
pointer colptr[N_COLS]      # I: The column pointers
pointer gp                  # I: graphics stream to draw instruments on
pointer ct                  # I: The coordinate transformation from 
                            #    arcsec to pixels.
bool    flip                # I: True if axes are flipped.

# Declarations.
pointer center           # The center coordinates in V2-V3 space.
pointer shape            # The name of the shape of the aperture.
pointer sp               # Stack pointer.
pointer x, tx, y, ty     # The vectors to draw the desired shape.

int npts                 # Number of points in the x,y plot vectors.
int row                  # The row being worked on.

bool none_found          # YES if no apertures were found.

# Function prototype.
int   si_find_aperture(), strcmp()

begin
	call smark (sp)
	call salloc (shape, SHAPE_SIZE, TY_CHAR)
	call salloc (center, N_DIM, TY_REAL)

	# Find the aperture in question.  This could be a wild card, thus,
	# keep going until there are no more matches.
	row = 0
	none_found = true
	while (si_find_aperture (siaf_tp, colptr[SIAP_ID], aperture_name, row)
	       != 0)
	{
	    # Indicate that at least one aperture was found.
	    none_found = false

	    # Get the shape of the aperture.
	    call tbegtt (siaf_tp, colptr[SHAPE], row, Memc[shape], SHAPE_SIZE)
	    call strlwr (Memc[shape])
	    
	    # Locate the center of the aperture.
	    if (flip) {
		call tbegtr (siaf_tp, colptr[SICS_V2],  row, ONER(center,Y_DIM))
		call tbegtr (siaf_tp, colptr[SICS_V3],  row, ONER(center,X_DIM))
	    } else {
		call tbegtr (siaf_tp, colptr[SICS_V2],  row, ONER(center,X_DIM))
		call tbegtr (siaf_tp, colptr[SICS_V3],  row, ONER(center,Y_DIM))
	    }

	    # This aperture is shaped like a circle.
	    if (0 == strcmp ("circ", Memc[shape]))
		call si_circle_form (siaf_tp, colptr, row, x, y, npts)

	    # This aperture is shaped like a rectangle.
	    else if (0 == strcmp ("rect", Memc[shape]))
		call si_rect_form (siaf_tp, colptr, row, x, y, npts)

	    # Aperture is a for sided figure- a quad.
	    else if (0 == strcmp ("quad", Memc[shape]))
		call si_quad_form (siaf_tp, colptr, row, x, y, npts)

	    # Aperture is a pickle.
	    else if (0 == strcmp ("pick", Memc[shape])) {
		call si_pickle_form (siaf_tp, colptr, row, x, y, npts)
		
		# Note, the center of the pickles is always V1.
		ONER(center,X_DIM) = 0.
		ONER(center,Y_DIM) = 0.

	    }

	    # Else, error with unknownn shape.
	    else {
		call eprintf ("ERROR: siaper: Unknown shape %s for aperture %s\n   skipping...\n")
		call pargstr (Memc[shape])
		call pargstr (aperture_name)
		next
	    }

	    # Create temporary vectors.
	    call malloc (tx, npts, TY_REAL)
	    call malloc (ty, npts, TY_REAL)

	    # Translate the object to its location.
	    if (flip)
		call sp_trans (ONER(y,1), ONER(x,1), npts, ONER(center,1), ONER(tx,1), 
			       ONER(ty,1))
	    else
		call sp_trans (ONER(x,1), ONER(y,1), npts, ONER(center,1), ONER(tx,1), 
			       ONER(ty,1))

	    # Now transform the coordinates to pixel space.
	    call mw_v2tranr (ct, ONER(tx,1), ONER(ty,1), ONER(x,1), ONER(y,1), npts)

	    # FINALLY, draw it.
	    call gpline (gp, ONER(x,1), ONER(y,1), npts)

	    call mfree (x, TY_REAL)
	    call mfree (y, TY_REAL)
	    call mfree (tx, TY_REAL)
	    call mfree (ty, TY_REAL)
	}

	# If no apertures were found, tell the user to shape up.
	if (none_found) {
	    call eprintf ("No apertures of the name %s was found.  Try it again!\n")
	    call pargstr (aperture_name)
	}

	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of si_plot_aperture
#---------------------------------------------------------------------------
