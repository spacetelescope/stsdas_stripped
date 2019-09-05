#---------------------------------------------------------------------------
.help ot.h 22Feb95 source
.ih
NAME
ot.h -- Memory structures and Macros for the waveid output table.
.endhelp
#---------------------------------------------------------------------------
#====
# OT structure.
define	OT_T		Memi[($1)+0]	# Table descriptor.
define	OT_COL_PTR	Memi[($1)+1]	# Column descriptors.
define	OT_CARPOS	Memi[($1)+2]	# Carrousel position.
define	OT_NROWS	Memi[($1)+3]	# Rows in table.
define	OT_APER_PTR	Memi[($1)+4]	# Aperture name.
define	OT_GRAT_PTR	Memi[($1)+5]	# Grating name.
define	OT_SZ		6

define	OT_APER		Memc[OT_APER_PTR($1)]
define	OT_COL		Memi[OT_COL_PTR($1)+($2)-1]
define	OT_GRAT		Memc[OT_GRAT_PTR($1)]
#====
# Column Ids.
define	C_LINE		1
define	C_SPORDER	2
define	C_PPOSP		3
define	C_PPOSO		4
define	C_WAVEO		5
define	C_SPOSP		6
define	C_SPOSO		7
define	C_DIFFW		8
define	C_DIFFP		9
define	C_DIFFS		10
define	C_INTP		11
define	OT_NCOLS	11
#---------------------------------------------------------------------------
# End of ot.h
#---------------------------------------------------------------------------
