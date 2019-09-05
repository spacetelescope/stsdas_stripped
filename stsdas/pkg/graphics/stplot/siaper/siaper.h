# siaper.h - Defines needed by the various routines of t_siaper.x
#
# History
#  3Apr91 - Extracted from t_siaper.x.  Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

# Define the default extent of the viewport.
define LOW_VIEW  0.05
define HIGH_VIEW 0.95

# Define macros to hide memory management.
define ONEC Memc[$1+$2-1]
define ONEI Memi[$1+$2-1]
define ONER Memr[$1+$2-1]
define ONED Memd[$1+$2-1]
define TWOD Memd[$1+($2*($4-1))+$3-1]

# Define conversion from Hours to Degrees.
define HRSTODEG ($1 * 15.)

# Indicators of whether RA (longitude) or DEC (latitude) is being read.
define LONGITUDE 1
define LATITUDE  2

# Define the number of segments needed to draw circles.
define N_ARC_SEGMENTS 50

# Number of points needed to define a rectangle.
define N_RECT 5

# Size of the SIAP_ID name.
define SIAP_ID_SIZE SZ_LINE

# The length of the shape names.
define SHAPE_SIZE 4

# Define the columns from the SIAF STSDAS table.
define N_COLS      20

define SIAP_ID      1
define SICS_V2      2
define SICS_V3      3
define SHAPE        4
define MAJ_AXIS     5
define MIN_AXIS     6
define ROT_ANGLE    7
define VRT1_X       8
define VRT1_Y       9
define VRT2_X      10
define VRT2_Y      11
define VRT3_X      12
define VRT3_Y      13
define VRT4_X      14
define VRT4_Y      15
define IN_ROT_ANG  16
define IN_ANG_EXT  17
define OUT_ROT_ANG 18
define OUT_ANG_EXT 19
define PARITY	   20

# Maximum dimensionality of an MWCS.
define MAX_DIM 7

# Dimensionality of an image.
define N_DIM   2

# Axes of an image.
define X_DIM   1
define Y_DIM   2

# Define how to get from Degrees to Seconds.
define DEGTOSEC ( ($1) * 3600 )

# Define MWCS types for get_axis_type_siaper.
define NUMBER_OF_SUPPORTED_TYPES 2
define RA_DEC_TAN                1
define LINEAR                    2
