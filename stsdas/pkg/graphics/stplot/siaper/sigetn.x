include <math.h>
include "siaper.h"

#---------------------------------------------------------------------------
# si_get_display_transformation - Determine how to transform aperture positions
#
# Description
#   The SIAF file describes the aperture positions in terms of arc-seconds
#   relative to the V1 axis of the OTA.  These coordinates need to be
#   transformed to correctly position and scale the apertures onto the
#   desired image.  This routine constructs a MWCS containing such a
#   transformation.
#
# History
#    6Mar91 - Created by Jonathan D. Eisenhamer, STScI.
#   17Nov03 - Remove all references to the LTM and LTV terms. peh
#---------------------------------------------------------------------------

procedure si_get_display_transformation( image_mw, center_ap, siaf_tp,
                                         colptr, roll, sky_project,
					 ra, dec, display_mw, flip )

pointer image_mw            # I:  The MWCS describing the image.
char    center_ap[ARB]      # I:  Name of the center aperture.
pointer siaf_tp             # I:  The SIAF table descriptor.
pointer colptr[N_COLS]      # I:  The column descriptors.
real    roll                # I:  The spacecraft roll (degrees).
bool	sky_project	    # I:  Invert V3 for sky projection?
real    ra, dec             # IO: Where the HST is pointing.
pointer display_mw          # O:  The display transformation.
bool    flip                # O:  True if axes are transposed.

# Declarations
pointer cd                  # The CD matrix of the Wterm of the image.
                            # Also used for intermediate results.
pointer r                   # The reference pixel coordinates.
pointer sp                  # Stack pointer.
pointer temp_str            # Temporary center aperture string.
pointer tmp_cd1             # Intermediate matrix.
pointer tmp_cd2             # Intermediate matrix.
pointer w                   # World coordinates of the reference pixel.

double d_roll               # Roll in radians.

int row                     # The row in the table of the center aperture.
int type                    # Coordinate system type of the image.

string logical "logical"    # String indicating the Logical coordinates.
string world   "world"      # String indication the World coordinates.

# Function prototype.
pointer mw_open(), mw_sctran()
int si_find_aperture()

begin
	call smark( sp )
	call salloc( temp_str, SZ_LINE, TY_CHAR )
	call salloc( cd, N_DIM*N_DIM, TY_DOUBLE )
	call salloc( tmp_cd1, N_DIM*N_DIM, TY_DOUBLE )
	call salloc( tmp_cd2, N_DIM*N_DIM, TY_DOUBLE )
	call salloc( r, N_DIM, TY_DOUBLE )
	call salloc( w, N_DIM, TY_DOUBLE )

	# Create a new MWCS.
	display_mw = mw_open( NULL, N_DIM )

	# Retrieve the Wterm of the image MWCS.  The display MWCS will use
	# the CD matrix which has the pixel size (at the tangent point).
	call si_get_axis_type( image_mw, type, ONED(r,1), ONED(w,1),
			       TWOD(cd,N_DIM,1,1), flip )

	# But, the CD matrix is (assumed) to be in degrees.  We need to convert
	# to arc-seconds.
	if (sky_project) {
	    TWOD(tmp_cd1,N_DIM,X_DIM, X_DIM) = -1 * DEGTOSEC( 1. )
	    TWOD(tmp_cd1,N_DIM,Y_DIM, Y_DIM) = -1 * DEGTOSEC( 1. )
	} else {
	    TWOD(tmp_cd1,N_DIM,X_DIM, X_DIM) = DEGTOSEC( 1. )
	    TWOD(tmp_cd1,N_DIM,Y_DIM, Y_DIM) = DEGTOSEC( 1. )
	}
	TWOD(tmp_cd1,N_DIM,X_DIM, Y_DIM) = 0.
	TWOD(tmp_cd1,N_DIM, Y_DIM, X_DIM) = 0.
	call mw_mmuld( TWOD(cd,N_DIM,1,1), TWOD(tmp_cd1,N_DIM,1,1),
		       TWOD(tmp_cd2,N_DIM,1,1), N_DIM )

	# Now invert it.
	call mw_invertd( TWOD(tmp_cd2,N_DIM,1,1), TWOD(tmp_cd1,N_DIM,1,1),
		N_DIM )

	# The reference point for the display MWCS is the pixel position of
	# the specified RA/DEC.  This must be run through the image MWCS to
	# retrieve.  If either RA or DEC is unspecified, then the reference
	# point in the Image MWCS is used.
	if( IS_INDEFR (ra) )
	    ra = ONED(w,X_DIM)
	if( IS_INDEFR (dec) )
	    dec = ONED(w,Y_DIM)
	call mw_c2trand( mw_sctran( image_mw, world, logical, 3b ),
			 double( ra ), double( dec ), ONED(w,X_DIM),
			 ONED(w,Y_DIM) )

	# The reference point is the V1 axis.
	ONED(r,X_DIM) = 0.
	ONED(r,Y_DIM) = 0.

	# Set the Wterm for the display MWCS.
	call mw_swtermd( display_mw, ONED(r,1), ONED(w,1), TWOD(tmp_cd1,N_DIM,1,1),
			 N_DIM )

	# The above was to create the transformation from the spacecraft's
	# aperture space defined in arcsec to the pixel space of the display.
	# Now, define the transformation that incorporates which aperture is
	# chosen to be the "center" (the one on the specified RA/DEC), and
	# the spacecrafts roll. Technically, this part of the transformation
	# is the Lterm of the display MWCS.

	# Determine where the center aperture is.
	call strcpy( "^", Memc[temp_str], 1 )
	call strcat( center_ap, Memc[temp_str], SZ_LINE )
	call strcat( "$", Memc[temp_str], SZ_LINE )
	row = 0
	if ( si_find_aperture( siaf_tp, colptr[SIAP_ID], Memc[temp_str], row ) == 0 )
	    call error( 1, "Unknown center aperture" )
	if( flip ) {
	    call tbegtd( siaf_tp, colptr[SICS_V2], row, ONED(r,Y_DIM) )
	    call tbegtd( siaf_tp, colptr[SICS_V3], row, ONED(r,X_DIM) )
	} else {
	    call tbegtd( siaf_tp, colptr[SICS_V2], row, ONED(r,X_DIM) )
	    call tbegtd( siaf_tp, colptr[SICS_V3], row, ONED(r,Y_DIM) )
	}

	# Incorporate the roll of the spacecraft.
	# Note:  The angle is negative because the relation between the V3 axis
	# and the spacecraft roll is essentially V3 = -ROLL
	# (unless transposed where the negative has already been taken care of).
	if( flip )
	    d_roll = double( DEGTORAD( roll ) )
	else
	    d_roll = -double( DEGTORAD( roll ) )
	TWOD(cd,N_DIM,X_DIM,X_DIM) =  cos(d_roll)
	TWOD(cd,N_DIM,Y_DIM,X_DIM) =  sin(d_roll)
	TWOD(cd,N_DIM,X_DIM,Y_DIM) = -sin(d_roll)
	TWOD(cd,N_DIM,Y_DIM,Y_DIM) =  cos(d_roll)

	# Set the Lterm.
	ONED(w,X_DIM) = 0.
	ONED(w,Y_DIM) = 0.
	call mw_translated( display_mw, ONED(w,1), TWOD(cd,N_DIM,1,1),
			    ONED(r,1), N_DIM )

	# That's all folks.
	call sfree( sp )
end
#---------------------------------------------------------------------------
# End of si_get_display_transformation
#---------------------------------------------------------------------------
