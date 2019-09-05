# Header file for the GSTATISTICS task.

define	LEN_GSTAT	25

define	GS_SUMX		Memd[P2D($1)]
define	GS_SUMX2        Memd[P2D($1+2)]
define  GS_SUMX3        Memd[P2D($1+4)]
define  GS_SUMX4        Memd[P2D($1+6)]
define	GS_LO		Memr[$1+8]
define	GS_HI		Memr[$1+9]
define	GS_MIN		Memr[$1+10]
define	GS_MAX		Memr[$1+11]
define	GS_MEAN		Memr[$1+12]
define	GS_MIDPT	Memr[$1+13]
define	GS_MODE		Memr[$1+14]
define	GS_STDDEV	Memr[$1+15]
define	GS_SKEW		Memr[$1+16]
define	GS_KURT   	Memr[$1+17]
define	GS_NPIX		Memi[$1+18]
define  GS_NBINS        Memi[$1+19]       # Number of bins of histogram
define  GS_HMIN         Memr[$1+20]       # Floor of histogram
define  GS_HMAX         Memr[$1+21]       # Ceiling of histogram
define  GS_HWIDTH       Memr[$1+22]       # Bin width of histogram
define  GS_HGM          Memi[$1+23]       # Array holding histogram

define  GS_FIELDS  "|doall|npix|min|max|sum|mean|midpt|mode|stddev|skew|kurt|"

define	NFIELDS		11

define	GS_FDOALL	1
define	GS_FNPIX	2
define	GS_FMIN		3
define	GS_FMAX		4
define	GS_FSUM		5
define	GS_FMEAN	6
define	GS_FMIDPT	7
define	GS_FMODE	8
define	GS_FSTDDEV	9
define	GS_FSKEW	10
define	GS_FKURT	11

define  LEN_SW          10               # Switches

define  SW_DOALL        Memi[$1]
define  SW_NPIX         Memi[$1+1]
define  SW_MNMX         Memi[$1+2]
define  SW_SUM          Memi[$1+3]
define  SW_MEAN         Memi[$1+4]
define  SW_STDDEV       Memi[$1+5]
define  SW_MIDPT        Memi[$1+6]
define  SW_MODE         Memi[$1+7]
define  SW_SKEW         Memi[$1+8]
define  SW_KURT         Memi[$1+9]

define  MSK_NONE        0                # No masks
define  MSK_LST         1                # Type of pixel lists masks
define  MSK_IMG         2                # Type of mask images

define  MAX_RANGES      100              # Max allowed number of groups ranges
define  GS_MITER        5                # Max number of rebinning operations
define  GS_HLIM         32768            # Max number of bins in histogram
