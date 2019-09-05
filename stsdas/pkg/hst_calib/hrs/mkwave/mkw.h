#---------------------------------------------------------------------------
.help mkw.h 11Apr95 source
.ih
NAME
mkw.h -- Structures for mkwave
.endhelp
#---------------------------------------------------------------------------
# Memory structure
define	MKW_WAVE_PTR		Memi[$1+0]		# Wavelength array.
define	MKW_SPEC_PTR		Memi[$1+1]		# Created spectrum.
define	MKW_LINES_PTR		Memi[$1+2]		# Wavelength line list.
define	MKW_INT_PTR		Memi[$1+3]		# Wavelength ints.
define	MKW_FWHM_PTR		Memi[$1+4]		# Widths.
define	MKW_NLINES		Memi[$1+5]		# Number of lines.
define	MKW_NPIX		Memi[$1+6]		# Number of pixels.
define	MKW_FNU			Memi[$1+7]		# FNU?
define	MKW_SUBSAMPLE		Memd[P2D($1+10)]	# Subsample size.
define	MKW_PEAK		Memd[P2D($1+12)]	# Random peak.
define	MKW_Z			Memd[P2D($1+14)]	# [Red)]Shift.
define	MKW_SIGMA		Memd[P2D($1+16)]	# Width of lines.
define	MKW_NSIGMA		Memd[P2D($1+18)]	# Number of sigma.
define	MKW_CONT		Memd[P2D($1+20)]	# Continuum.
define	MKW_SLOPE		Memd[P2D($1+22)]	# Slope.
define	MKW_TEMP		Memd[P2D($1+24)]	# Temperature.
define	MKW_SZ			26			# Size of structure.

#----
# Structure access.
define	MKW_WAVE		Memd[MKW_WAVE_PTR($1)+$2-1]
define	MKW_SPEC		Memr[MKW_SPEC_PTR($1)+$2-1]
define	MKW_LINES		Memd[MKW_LINES_PTR($1)+$2-1]
define	MKW_INT			Memr[MKW_INT_PTR($1)+$2-1]
define	MKW_FWHM		Memr[MKW_FWHM_PTR($1)+$2-1]
#---------------------------------------------------------------------------
# End of mkw.h
#---------------------------------------------------------------------------
