#################################################################################
#  nmcom.h	Include file of the COMMON block for the noisemodel task	#
#										#
#	8/91	RAShaw: Initial SPP code					#

define	BINMIN		3
define	HELP		"wfpc$/noisemodel.key"
#define	HELP		"home$source/noisemodel.key"
define	PROMPT		"wfpc.noisemodel cursor options"

#  Parameters:	
real	cliphi			# Chi-square upper rejection threshold
real	cliplo			# Chi-square lower rejection threshold
real	gain			# Inverse detector gain in e-/DN
real	readn			# CCD read noise in DN
real	scalen			# Scale noise, in percent of DN
int	xypts			# Number of pixels per bin

#///////////////////////////////////////////////////////////////////////////////#
common	/nmparm/	cliphi, cliplo, gain, readn, scalen, xypts
