# Header file for the WSTATISTICS task.

define	LEN_WSTAT	14

define	WS_SUMX		Memd[P2D($1)]
define	WS_LO		Memr[$1+2]
define	WS_HI		Memr[$1+3]
define	WS_MIN		Memr[$1+4]
define	WS_MAX		Memr[$1+5]
define	WS_MEAN		Memr[$1+6]
define	WS_MEDIAN	Memr[$1+7]
define	WS_MODE		Memr[$1+8]
define	WS_STDDEV	Memr[$1+9]
define	WS_SKEW		Memr[$1+10]
define	WS_KURT		Memr[$1+11]
define	WS_NPIX		Memi[$1+12]

define	nxtgrp_		10
