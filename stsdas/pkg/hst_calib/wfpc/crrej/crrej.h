define	VERSION		"2.2 (Apr 25, 2003)"

define	MAX_ITER	20
define	MAX_FILES	120

define	MAX_NEXP	3
define	NONEXIST	-10000.

define	IMAGE		1
define	MINIMUM		2
define	MEDIAN		3

define	NORMAL		0
define	IN_DQF		1
define	OUT_DQF		2
define	OUT_IMG		4

# define the parameter structure
define 	LEN_PAR		10+(SZ_LINE+1)*20

define	LOWER		Memr[($1)]
define 	UPPER		Memr[($1+1)]
define 	REJ		Memr[($1+2)]
define 	PSIGMA		Memr[($1+3)]
define 	FILLVAL		Memr[($1+4)]
define 	NEXPNAMES	Memi[($1+5)]
define	CRVAL		Memi[($1+6)]
define	BADBITS		Memi[($1+7)]
define 	VERBOSE		Memb[($1+9)]

define 	SIGMAS		Memc[P2C($1+10)]
define 	INITIAL		Memc[P2C($1+10+(SZ_LINE+1))]
define 	SKY		Memc[P2C($1+10+(SZ_LINE+1)*2)]
define 	READNOISE	Memc[P2C($1+10+(SZ_LINE+1)*3)]
define 	ATODGAIN	Memc[P2C($1+10+(SZ_LINE+1)*4)]
define 	SCALENOISE	Memc[P2C($1+10+(SZ_LINE+1)*5)]
define 	EXPNAME		Memc[P2C($1+10+(SZ_LINE+1)*6)]
define 	EXPNAME2	Memc[P2C($1+10+(SZ_LINE+1)*7)]
define 	EXPNAME3	Memc[P2C($1+10+(SZ_LINE+1)*8)]
define 	SKYNAME		Memc[P2C($1+10+(SZ_LINE+1)*11)]
