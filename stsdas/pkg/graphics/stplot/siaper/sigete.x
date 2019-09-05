include <mwset.h>
include "siaper.h"

# si_get_axis_type - Determine type of transformation the MWCS represents.
#
# Bugs
#  - A potential problem:  For a WCS that has more axes than necessary
#    for the sky projections, those axis are set such that during
#    transformations, the first index position is used.  For the one
#    example I have seen, the "third" axis is time and this interpretation
#    works.  But, I am sure something will fall apart because of this.
#
# History
#   3Dec90 - Created by Jonathan D. Eisenhamer, STScI.
#   2Jan91 - Put in the axis mapping procedures to handle reveresed/multi-
#            dimensional situations. jde
#  21Jan91 - Reworked the axis and type determination based on a better
#            understanding of how MWCS expects things. jde
#   7Feb91 - Added ability to recognize linear WCS'. jde
#   6Mar91 - Taken from t_wcslab and modified slightly for t_siaper. jde
#  20Mar91 - Modified to allocate, deallocate the necessary memory. jde
#   3Jul91 - Added flag to indicate when axes are transposed. jde
#  17Nov03 - Remove all references to the LTM and LTV terms. peh
#---------------------------------------------------------------------------

procedure si_get_axis_type( mw, axis_type, logical_center, world_center,
                                cd, flip )

pointer mw                    # I: The MWCS descriptor.
int axis_type                 # O: The transformation type:
                              #  RA_DEC_TAN -> Tangential view in Right
                              #               Ascension and Declination.
                              #  LINEAR     -> Any regular linear system.
                              #  INDEFI     -> The type of transformation could
                              #          not be determined.
double logical_center[N_DIM]  # O: The center point in the logical system.
double world_center[N_DIM]    # O: The center point in the world system.
double cd[N_DIM, N_DIM]       # O: The CD matrix of the Wterm.
bool   flip                   # O: True if the axes have been transposed.

# Declarations.
pointer axno                  # Axis mapping.
pointer axtype                # The axis type.
pointer axval                 # Default axis.
pointer cur_type              # String containing the current axis type
                              # being looked for.
pointer found_axis_list       # Array containg the axis numbers of useful
                              # axes.
pointer sp                    # Stack pointer.
pointer tmp_cd                # Temporary CD matrix.
pointer tmp_logical           # The temporary array for the center point
                              # in the Logical system.
pointer tmp_world             # The temporary array for the center point
                              # in the World system.


int axis                      # Axis index.
int found_axis                # Number of WCS axes that match the current type
int index_lat                 # Index that represents the latitude.
int index_long                # Index that represents the longitude.
int wcs_dim                   # The dimensionality of the WCS.

# Function declarations.
int mw_stati(), strncmp(), strlen()

errchk mw_gwattrs

begin
	call smark( sp )
	call salloc( found_axis_list, N_DIM, TY_INT )
	call salloc( axtype, SZ_LINE, TY_CHAR )
	call salloc( cur_type, SZ_LINE, TY_CHAR )

	# Get the dimensionality of the WCS.
	wcs_dim = mw_stati( mw, MW_NDIM )

	# Allocate the arrays based on the dimensionality.
	call salloc( tmp_cd, wcs_dim*wcs_dim, TY_DOUBLE )
	call salloc( tmp_logical, wcs_dim, TY_DOUBLE )
	call salloc( tmp_world, wcs_dim, TY_DOUBLE )
	call salloc( axno, wcs_dim, TY_INT )
	call salloc( axval, wcs_dim, TY_INT )

	# Initialize the two dimensions.
	index_long = INDEFI
	index_lat = INDEFI

	# Look through the possible supported axis types.  When a type has
	# exactly N_DIM axes defined, that will be the one used.
	for( axis_type = 1; axis_type <= NUMBER_OF_SUPPORTED_TYPES;
	     axis_type = axis_type + 1 ) {

		 # Determine the string that should be looked for.
		 switch( axis_type ) {
		 case RA_DEC_TAN: call strcpy( "tan", Memc[cur_type], 3 )
		 case LINEAR:     call strcpy( "linear", Memc[cur_type], 6 )
		 }

		 # Initialize the number of found axes.
		 found_axis = 0

		 # Examine each axis to determine whether the current axis
		 # type is the one to use.
		 for( axis = 1; axis <= wcs_dim; axis = axis + 1 ) {
		     ifnoerr( call mw_gwattrs( mw, axis, "wtype",
					       Memc[axtype], SZ_LINE ) ) {
			 call strlwr( Memc[axtype] )

			 # If this axis type matches the one being looked for,
			 # add it to the axis list.  If there are too many
			 # axes of the current type found, don't add to the
			 # found axis list.
			 if( strncmp( Memc[axtype] , Memc[cur_type],
				      strlen( Memc[cur_type] ) ) == 0 ){
			     found_axis = found_axis + 1
			     if( found_axis <= N_DIM )
				 ONEI(found_axis_list,found_axis) = axis
			 }
		     }

		     # Else, if no wtype is found, assume linear
		     else if (axis_type == LINEAR) {
			 found_axis = found_axis + 1
			 if (found_axis <= N_DIM)
			     ONEI(found_axis_list,found_axis) = axis
		     }
		 }

		 # Check to see whether we have the right number axes.
		 if ( found_axis == N_DIM )
		     break
	     }

	# If any axis were found, then further check axis types.
	# Depending on the axis type, there may be need to distinguish between
	# the two possible axes further.
	if ( found_axis == N_DIM )
	    switch( axis_type ) {
	    case RA_DEC_TAN:
		for( axis = 1; axis <= N_DIM; axis = axis + 1 )
		    ifnoerr( call mw_gwattrs( mw, ONEI(found_axis_list,axis),
					      "axtype", Memc[axtype],
					      SZ_LINE ) ) {
			call strlwr( Memc[axtype] )
			if( strncmp( Memc[axtype], "ra", 2 ) == 0 )
			    index_long = ONEI(found_axis_list,axis)
			else if( strncmp( Memc[axtype], "dec", 3 ) == 0 )
			    index_lat = ONEI(found_axis_list,axis)
		    }		

	    default:

		# The "default" seems to be the LINEAR case for MWCS.
		# Since no other information is provided, this is all we know.
		index_long = ONEI(found_axis_list,X_DIM)
		index_lat = ONEI(found_axis_list,Y_DIM)

	    }

	# If either axis is unknown, something is wrong.  If the WCS has two
	# axes defined, then make some grand assumptions.  If not, then there
	# is nothing more to be done.
	if ( IS_INDEFI( index_long ) || IS_INDEFI( index_lat ) ) {
	    if ( wcs_dim == N_DIM ) {
		call eprintf(
			     "get_axis_type: Could not determine WCS type, assuming linear!\n" )
		index_long = X_DIM
		index_lat = Y_DIM

	    } else
		call error( 1, "get_axis_type: Insufficient axes, wcslab requires 2" )
	}


	# Zero the axis values and set any "unknown" axis to always use the
	# "first" position in that axis direction.  This will more than likely
	# be a problem, but no general solution comes to mind this second.
	call amovki( 0, ONEI(axno,1), wcs_dim )
	call amovki( 1, ONEI(axval,1), wcs_dim )

	# Set up the mapping such the Longitude is the "X" dimension and
	# Latitude is the "Y" dimension.
	ONEI(axno,index_long) = X_DIM
	ONEI(axno,index_lat) = Y_DIM
	call mw_saxmap( mw, ONEI(axno,1), ONEI(axval,1), wcs_dim )

	# Recover the center points of the Logical and World systems.
	call mw_gwtermd (mw, ONED(tmp_logical,1), ONED(tmp_world,1),
			 TWOD(tmp_cd,wcs_dim,1,1), wcs_dim )

	logical_center[X_DIM] = ONED(tmp_logical,index_long)
	logical_center[Y_DIM] = ONED(tmp_logical,index_lat)

	world_center[X_DIM] = ONED(tmp_world,index_long)
	world_center[Y_DIM] = ONED(tmp_world,index_lat)

	cd[X_DIM, X_DIM] = TWOD(tmp_cd,wcs_dim,index_long,index_long)
	cd[X_DIM, Y_DIM] = TWOD(tmp_cd,wcs_dim,index_long,index_lat)
	cd[Y_DIM, Y_DIM] = TWOD(tmp_cd,wcs_dim,index_lat,index_lat)
	cd[Y_DIM, X_DIM] = TWOD(tmp_cd,wcs_dim,index_lat,index_long)

	# Check for transposition of axes.
	flip = ( index_long > index_lat )

	# That's all folks.
	call sfree( sp )
end
#---------------------------------------------------------------------------
# End of si_get_axis_type
#---------------------------------------------------------------------------
