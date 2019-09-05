include <math.h>		# for RADTODEG

# nor_non_foc -- get the orientation for a non-FOC image
#
# Phil Hodge,  2-Aug-1993  Subroutine created.

procedure nor_non_foc (im, angle, range,
		orientat, cd_one, cd_two, parity, maxch,
		comment, max_comment)

pointer im			# i: imhdr pointer for science file or NULL
double	angle			# o: one number representing orientation angle
double	range			# o: range of orientation values
double	orientat		# o: value of keyword from science file
double	cd_one, cd_two		# o: two values of orientation from CD matrix
char	parity[ARB]		# o: normal or reversed (or unknown)
int	maxch			# i: max length of parity
char	comment[ARB]		# o: comments about corrections to angles
int	max_comment		# i: max length of comment
#--
double	cd[2,2]			# CD matrix
double	determinant		# determinant of the CD matrix
double	minval, maxval		# min & max of all orientation angles
bool	got_orientat		# have we got a value for orientat?
bool	got_cd			# have we got values for the CD matrix?
double	imgetd()
int	imaccf()

begin
	if (im == NULL)
	    return

	# Initial values.
	got_orientat = false
	got_cd = false

	if (imaccf (im, "orientat") == YES) {
	    orientat = imgetd (im, "orientat")
	    if (orientat < 0.d0)
		orientat = orientat + 360.d0
	    else if (orientat >= 360.d0)
		orientat = orientat - 360.d0
	    got_orientat = true
	}

	if (imaccf (im, "cd1_1") == YES) {
	    cd[1,1] = imgetd (im, "cd1_1")
	    got_cd = true
	    if (imaccf (im, "cd2_2") == YES)
		cd[2,2] = imgetd (im, "cd2_2")
	    else
		got_cd = false		# must have both cd1_1 & cd2_2
	    if (imaccf (im, "cd1_2") == YES)
		cd[1,2] = imgetd (im, "cd1_2")
	    else
		cd[1,2] = 0.d0
	    if (imaccf (im, "cd2_1") == YES)
		cd[2,1] = imgetd (im, "cd2_1")
	    else
		cd[2,1] = 0.d0
	}

	if (got_cd) {
	    # Calculate the orientation from the CD matrix.
	    # cd_one is from a vector pointing in the north direction;
	    # cd_two is from a vector pointing in the east direction.

	    determinant = cd[1,1] * cd[2,2] - cd[1,2] * cd[2,1]
	    if (determinant < 0) {
		call strcpy ("normal", parity, maxch)
		cd_one = RADTODEG (atan2 (cd[2,1], -cd[1,1]))
		cd_two = RADTODEG (atan2 (cd[1,2],  cd[2,2]))
	    } else if (determinant > 0) {
		call strcpy ("reversed", parity, maxch)
		cd_one = RADTODEG (atan2 (-cd[2,1], cd[1,1]))
		cd_two = RADTODEG (atan2 ( cd[1,2], cd[2,2]))
	    } else {
		got_cd = false
		call strcpy ("|CD| = 0", parity, maxch)
		call strcat (" |CD| = 0;", comment, max_comment)
	    }
	}

	if (got_cd) {
	    if (cd_one < 0.d0)
		cd_one = cd_one + 360.d0
	    if (cd_one >= 360.d0)
		cd_one = cd_one - 360.d0
	    if (cd_two < 0.d0)
		cd_two = cd_two + 360.d0
	    if (cd_two >= 360.d0)
		cd_two = cd_two - 360.d0
	}

	# Get one number to represent the best value for the image orientation.
	if (got_orientat)
	    angle = orientat
	else if (got_cd)
	    angle = (cd_one + cd_two) / 2.d0
	else
	    angle = INDEFD

	# Find the range of values.
	if (IS_INDEFD(angle)) {
	    range = INDEFD
	} else {
	    minval = angle
	    maxval = angle
	    if (got_orientat) {
		minval = min (minval, orientat)
		maxval = max (maxval, orientat)
	    }
	    if (got_cd) {
		minval = min (minval, cd_one)
		maxval = max (maxval, cd_one)
		minval = min (minval, cd_two)
		maxval = max (maxval, cd_two)
	    }
	    range = maxval - minval
	    if (range > 180.d0)		# e.g. maxval = 359, minval = 1
		range = minval + 360.d0 - maxval
	}
end
