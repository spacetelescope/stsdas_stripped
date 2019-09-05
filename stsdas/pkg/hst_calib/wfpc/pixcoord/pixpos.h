# Copyright restrictions apply - see stsdas$copyright.stsdas 
#
# PIXPOS.H -- Constants and structures used by pixpos

define	POS_LENGTH	11
define	SZ_STARNAME	63

define	POS_NSTAR	Memi[$1]
define	POS_XLEN	Memi[$1+1]
define	POS_YLEN	Memi[$1+2]
define	POS_NAMEVEC	Memi[$1+3]
define	POS_FLAGVEC	Memi[$1+4]
define	POS_RAVEC	Memi[$1+5]
define	POS_DECVEC	Memi[$1+6]
define	POS_XOLDVEC	Memi[$1+7]
define	POS_YOLDVEC	Memi[$1+8]
define	POS_XNEWVEC	Memi[$1+9]
define	POS_YNEWVEC	Memi[$1+10]

define	POS_NAME	Memc[POS_NAMEVEC($1)+(SZ_STARNAME+1)*($2-1)]
define	POS_FLAG	Memi[POS_FLAGVEC($1)+$2-1]
define	POS_RA		Memd[POS_RAVEC($1)+$2-1]
define	POS_DEC		Memd[POS_DECVEC($1)+$2-1]
define	POS_XOLD	Memd[POS_XOLDVEC($1)+$2-1]
define	POS_YOLD	Memd[POS_YOLDVEC($1)+$2-1]
define	POS_XNEW	Memd[POS_XNEWVEC($1)+$2-1]
define	POS_YNEW	Memd[POS_YNEWVEC($1)+$2-1]

define	POS_PARAM	1
define	POS_TEXT	2
define	POS_INT		3
define	POS_DBL		4

define	FLAG_IN		0	# Star is in plate solution
define	FLAG_OUT	1	# Star is not in plate solution
define	FLAG_OFF	2	# Star is off plate
define	FLAG_NULL	3	# Don't know or care where star is

define	MAX_SOLUTION	6	# Number of terms in least squares solution
