#---------------------------------------------------------------------------
.help wid.h 10Apr95 source
.ih
NAME
wid.h -- Memory structures and Macros for t_waveid
.endhelp
#---------------------------------------------------------------------------
#====
# WID structure.
define	WID_OFF		Memr[$1+0]	# Known zeropoint offset.
define	WID_UNITS	Memi[$1+1]	# Units of the offset.
define	WID_NO_DUPS	Memi[$1+4]	# YES to remove duplicate finds.
define	WID_OBS_PTR	Memi[$1+5]	# Observation data.
define	WID_S0		Memr[$1+6]	# Sample position @ pixel 1.
define	WID_SD		Memr[$1+7]	# Sample per pixel.
define	WID_WAVE_PTR	Memi[$1+8]	# Wavelength solution.
define	WID_NPIX	Memi[$1+9]	# Number of pixels in obs/wave data.
define	WID_WIDTH	Memr[$1+10]	# Width of emission feature.
define	WID_RADIUS	Memr[$1+11]	# Search radius.
define	WID_THRESH	Memr[$1+12]	# Minimum pixel value.
define	WID_INTER	Memi[$1+13]	# YES to be interactive.
define	WID_MSHIFT	Memi[$1+14]	# Maximum xcor shift.
define	WID_XCOR	Memi[$1+15]	# YES to do initial xcor.
define	WID_M		Memi[$1+16]	# Spectral order.
define	WID_SZ		17

define	WID_OBS		Memr[WID_OBS_PTR($1)+$2-1]
define	WID_WAVE	Memd[WID_WAVE_PTR($1)+$2-1]

#====
# UNITS Dictionary
define	WID_UNITS_DICT		",pixel,wavelength,sample"
define	WID_UNITS_PIXEL		1
define	WID_UNITS_WAVE		2
define	WID_UNITS_SAMPLE	3
#---------------------------------------------------------------------------
# End of wid.h
#---------------------------------------------------------------------------
