# newcont.h - Include file definitions for t_newcont
#
# History
#  21Jan91 - Created by Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

# Define the modes for farb2d.  See the routine farb2d for more information.
define  MODE_DICT       "|fill|line"
define  MODE_FILL_AREA  0
define  MODE_LINE_DRAW  1

# Define the possible line styles.
define SOLID_THIN    1
define SOLID_THICK   2
define DOTTED_THIN   3
define DOTTED_THICK  4
define DASHED_THIN   5
define DASHED_THICK  6
define DOTDASH_THIN  7
define DOTDASH_THICK 8
define BOLD          9

# Define how thick and thin the lines should be
define CONTOUR_THIN  1.
define CONTOUR_THICK 1.5

# Define how many contours to use if none are specified and the maximum
# number possible.
define APPROXIMATE_CONTOURS 16
define MAXIMUM_CONTOURS 100

# Define text size for high/low marks.
define SMALL_SIZE 0.5

# Define the possible states for parsing the contour list input.
define NEXT_INPUT 1
define GET_COLOR  2

# Define the possible functions to determine contours.
define FUNC_LINEAR 1
define FUNC_LOG    2
define FUNC_RLOG   3

# Setup parameters for drawing the contour level scale.
define  UNUSED_WCS              15
define  BOX_RIGHT_DISPLACEMENT  0.05    # 5%  of the size of the plot
define  BOX_SIZE_X              0.05    # 5%  "   "   "   "   "   "
define  BOX_UP_DISPLACEMENT     0.1     # 10% "   "   "   "   "   "
define  BOX_DOWN_DISPLACEMENT   0.1     # 10% "   "   "   "   "   "
define  TEXT_OFFSET             0.7     # 50% "   "   "   "   "   "
define  TEXT_SIZE               1.1     # 110%"   "   "   "   "   "
define  LINE_WIDTH              5.      # 500%"   "   "   "   "   "
define  BOX_EXTRA               0.01    # 1%  "   "   "   "   "   "
