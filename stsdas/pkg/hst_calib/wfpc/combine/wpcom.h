#################################################################################
#  wpcom.h	Include file of COMMON blocks for COMBINE software.		#
#										#
#		Development version:	11/90	RAShaw				#
#										#
#################################################################################

int	BADBITS			# Flagged DQF bits
int	CRBIT			# Bit value for cosmic ray hit
real	BLANK			# Value to use if all pixels are rejected
real	SCALES[IMS_MAX]		# Scale factors for each image
real	WTS[IMS_MAX]		# Weights @each image; sum is normalized to one
real	ZEROS[IMS_MAX]		# Zero-point offsets for each image

#################################################################################

common	/basics/	SCALES, WTS, ZEROS, BADBITS, BLANK, CRBIT
