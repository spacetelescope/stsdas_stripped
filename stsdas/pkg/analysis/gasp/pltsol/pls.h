# PLS.H -- Plate solution program internal definitions

# WARNING: This include file together with the plsfit.h depend
#          on the structure defined in pkg$xtools/icfit/icgit,h

define  NTERMS_MODEL	20
define	MAX_NO_STARS	100
define  TERML		15

define	S2C		((($1)-1)*SZ_STRUCT+1)	# struct ptr to char ptr
define	S2D		((($1)-1)*SZ_STRUCT/SZ_DOUBLE+1)	# to double
define	S2X		((($1)-1)*SZ_STRUCT/SZ_COMPLEX+1)	# to complex


define  LEN_STRUCT	16
define	X_PREF		Memi[$1]		# Pointer to X coordinates
define	Y_PREF		Memi[$1+1]		# Pointer to Y coordinates
define	PRA		Memi[$1+2]		# Pointer to RA values
define	PDEC		Memi[$1+3]		# Pointer to DEC values
define  PMAG		Memi[$1+4]		# Pointer to magnitude values
define	PCOL		Memi[$1+5]		# Pointer to color values
define	PXI		Memi[$1+6]		# Pointer to XI coordinates
define	PETA		Memi[$1+7]		# Pointer to ETA coordinates
define	PW		Memi[$1+8]		# Pointer to coor weights
define	PXIC		Memi[$1+9]		# Pointer to xi regression val.
define	PETAC		Memi[$1+10]		# Pointer to eta regression val.
define	POFLAG		Memi[$1+11]		# Pointer to coord flag
define	XPA		Memi[$1+12]		# Pointer to XA
define	YPA		Memi[$1+13]		# Pointer to YA

define  X_REF		Memd[X_PREF($1)]	# X coor in mm w/r to center
define  Y_REF		Memd[Y_PREF($1)]	# Y coor in mm w/r to center
define	RA		Memd[PRA($1)]		# ra of object
define  DEC		Memd[PDEC($1)]		# dec of object
define  MAG		Memr[PMAG($1)]		# magnitud of object
define  COLOR		Memr[PCOL($1)]		# color of object
define  XI		Memd[PXI($1)]		# xi of object
define  ETA		Memd[PETA($1)]		# eta of object
define  WEIGHT		Memr[PW($1)]		# w for each object
define  XI_CALC		Memd[PXIC($1)]		# xi after regression
define  ETA_CALC	Memd[PETAC($1)]		# eta after regression
define  OFLAG		Memi[POFLAG($1)]	# Type of object flag
define	XA		Memd[XPA($1)]	# default X model
define	YA		Memd[YPA($1)]	# default Y model


