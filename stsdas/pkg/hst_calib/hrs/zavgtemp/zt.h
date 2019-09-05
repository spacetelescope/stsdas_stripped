#---------------------------------------------------------------------------
.help zt.h 3Apr95 source
.ih
NAME
zt.h -- Memory structures/definitions for the zt object.
.endhelp
#---------------------------------------------------------------------------
#====
# Keys object.
define	ZT_K_KEY_PTR		Memi[($1)+0]	# Keywords to accumulate.
define	ZT_K_NKEY		Memi[($1)+1]	# Number of keywords.
define	ZT_K_A_PTR		Memi[($1)+2]	# Accumulator.
define	ZT_K_NA_PTR		Memi[($1)+3]	# Number of values accumed.
define	ZT_K_KEY_SZ		Memi[($1)+4]	# Length of key string.
define	ZT_K_SZ			5

define	ZT_K_KEY		Memc[ZT_K_KEY_PTR($1)]
define	ZT_K_A			Memd[ZT_K_A_PTR($1)+$2-1]
define	ZT_K_NA			Memi[ZT_K_NA_PTR($1)+$2-1]

#====
# Output table descriptor.
define	ZT_OT			Memi[($1)+0]	# Table descriptor.
define	ZT_OT_COL_PTR		Memi[($1)+1]	# Column descriptors.
define	ZT_OT_NCOLS		Memi[($1)+2]	# Number of columns.
define	ZT_OT_NROWS		Memi[($1)+3]	# Number of rows.
define	ZT_OT_SZ		4

define	ZT_OT_COL		Memi[ZT_OT_COL_PTR($1)+$2-1]
#---------------------------------------------------------------------------
# End of zt.h
#---------------------------------------------------------------------------
