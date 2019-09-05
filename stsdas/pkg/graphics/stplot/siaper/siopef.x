include "siaper.h"

# si_open_siaf - Open the SIAF SDAS table.
#
# History
#    8Mar91 - Created by Jonathan D. Eisenhamer
#---------------------------------------------------------------------------

procedure si_open_siaf( siaf_name, siaf_tp, colptr )

char    siaf_name[ARB]   # I:  The name of the SIAF SDAS table.
pointer siaf_tp          # O:  The table descriptor.
pointer colptr[N_COLS]   # O:  The array of column pointers.

# Declarations
int i                    # Column counter.

# Function prototypes
pointer tbtopn()

errchk tbtopn

begin

	# Open the SDAS table.
	siaf_tp = tbtopn( siaf_name, READ_ONLY, NULL )

	# Find the colum pointers. 
	call tbcfnd( siaf_tp, "SIAP_ID",     colptr[SIAP_ID],     1 )
	call tbcfnd( siaf_tp, "SICS_V2",     colptr[SICS_V2],     1 )
	call tbcfnd( siaf_tp, "SICS_V3",     colptr[SICS_V3],     1 )
	call tbcfnd( siaf_tp, "SHAPE",       colptr[SHAPE],       1 )
	call tbcfnd( siaf_tp, "MIN_AXIS",    colptr[MIN_AXIS],    1 )
	call tbcfnd( siaf_tp, "MAJ_AXIS",    colptr[MAJ_AXIS],    1 )
	call tbcfnd( siaf_tp, "ROT_ANGLE",   colptr[ROT_ANGLE],   1 )
	call tbcfnd( siaf_tp, "IN_ROT_ANG",  colptr[IN_ROT_ANG],  1 )
	call tbcfnd( siaf_tp, "IN_ANG_EXT",  colptr[IN_ANG_EXT],  1 )
	call tbcfnd( siaf_tp, "OUT_ROT_ANG", colptr[OUT_ROT_ANG], 1 )
	call tbcfnd( siaf_tp, "OUT_ANG_EXT", colptr[OUT_ANG_EXT], 1 )
	call tbcfnd( siaf_tp, "VRT1_X",      colptr[VRT1_X],      1 )
	call tbcfnd( siaf_tp, "VRT1_Y",      colptr[VRT1_Y],      1 )
	call tbcfnd( siaf_tp, "VRT2_X",      colptr[VRT2_X],      1 )
	call tbcfnd( siaf_tp, "VRT2_Y",      colptr[VRT2_Y],      1 )
	call tbcfnd( siaf_tp, "VRT3_X",      colptr[VRT3_X],      1 )
	call tbcfnd( siaf_tp, "VRT3_Y",      colptr[VRT3_Y],      1 )
	call tbcfnd( siaf_tp, "VRT4_X",      colptr[VRT4_X],      1 )
	call tbcfnd( siaf_tp, "VRT4_Y",      colptr[VRT4_Y],      1 )
	call tbcfnd( siaf_tp, "PARITY",      colptr[PARITY],      1 )

	# Check to make sure that all the needed columns are present.
	do i = 1, N_COLS
	    if ( colptr[i] == NULL )
		call error( 1, "data column missing from SIAF table" )
end
#---------------------------------------------------------------------------
# End of si_open_siaf
#---------------------------------------------------------------------------
