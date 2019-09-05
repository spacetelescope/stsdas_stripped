#---------------------------------------------------------------------------
.help line.h 13Mar95 source
.ih
NAME
line.h -- Definition of a Line Object.
.endhelp
#---------------------------------------------------------------------------
#====
# Line List
define	LL_N		Memi[($1)+0]		# Number of lines in list.
define	LL_MAXN		Memi[($1)+1]		# Maximum lines in list.
define	LL_WAVE_PTR	Memi[($1)+2]		# Line wavelength.
define	LL_INTP_PTR	Memi[($1)+3]		# Intensity (predicted)
define	LL_POSP_PTR	Memi[($1)+4]		# Position (pixel, predicted)
define	LL_POSO_PTR	Memi[($1)+5]		# Position (pixel, observed)
define	LL_WAVEO_PTR	Memi[($1)+6]		# Wavelength (observed)
define	LL_SZ		7

define	LL_WAVE		Memd[LL_WAVE_PTR($1)+$2-1]
define	LL_INTP		Memr[LL_INTP_PTR($1)+$2-1]
define	LL_POSP		Memr[LL_POSP_PTR($1)+$2-1]
define	LL_POSO		Memr[LL_POSO_PTR($1)+$2-1]
define	LL_WAVEO	Memd[LL_WAVEO_PTR($1)+$2-1]

define	LL_GROW		100
#---------------------------------------------------------------------------
# End of line.h
#---------------------------------------------------------------------------
