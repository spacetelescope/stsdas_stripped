#################################################################################
#  wpdef.h	Include file of parameters for COMBINE software.		#
#										#
#		Development version:	11/90	RAShaw				#
#										#
#################################################################################

define  IMS_MAX         64      # Maximum number of images which are mapped at	#
                                # the same time.				#

# WCOMBINE OPTION dictionary:

define	COMBINE "|sum|average|median|minreject|maxreject|minmaxreject|crreject|\
        |threshold|sigclip|avsigclip|"
define  SUM		1	# Sum of the images
define  AVERAGE		2	# Average of the images
define  MEDIAN		3	# Median of the images
define  MINREJECT	4	# Reject minimum
define  MAXREJECT	5	# Reject maximum
define  MINMAXREJECT	6	# Reject minimum and maximum
define  CRREJECT	7	# Cosmic ray rejection algorithm
#	newline		8
define  THRESHOLD	9	# Absolute threshold clip
define  SIGCLIP		10	# Sigma clip using sigma at each point
define  AVSIGCLIP	11	# Sigma clip using average sigma
