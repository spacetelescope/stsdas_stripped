define	VERSION		"1.3 (Feb 9, 1998)"

define	MAX_FILES	120

# define the parameter structure
define 	LEN_PAR		5+(SZ_LINE+1)*3

define	LOWER		Memr[($1)]
define 	UPPER		Memr[($1+1)]
define	BADBITS		Memi[($1+2)]
define 	VERBOSE		Memb[($1+3)]
define 	SUBSKY		Memb[($1+4)]
define 	SKYNAME		Memc[P2C($1+5)]
define  BUNIT         	Memc[P2C($1+5+(SZ_LINE+1))]
define  EXPNAME         Memc[P2C($1+5+(SZ_LINE+1)*2)]
define	SUBSKY_KEYW	"SUBSKY"
