# local definition for short header output information

define	LEN_SINFO	60

define	BITPIX		Memi[$1]	# Bits per pixel (Must be an MII type)
define  EXTEND		Memi[$1+1]	# Value of EXTEND keyword
define  XTENSION	Memi[$1+2]	# Standard extension flags. (see below)
define  NCOLS		Memi[$1+3]	# Number of columns in table
define  DATATYPE	Memi[$1+4]	# Get datatype from IRAFTYPE or DATATYP
define  NAXIS		Memi[$1+5]	# image dimension
define  NAXISN		Memi[$1+$2+6]   # Axis size
define  PCOUNT		Memi[$1+15]   # Axis size
define  GCOUNT		Memi[$1+16]   # Axis size
define  EXT_NUMBER      Memi[$1+17]   # Axis size
define  EXTVER          Memi[$1+18]   # Axis size
define  IRAFNAME	Memc[P2C($1+19)]  # Object name, 70 chars

define	SZ_OBJECT	70
define  CAT_AREA	(P2C($1+LEN_SINFO))
define  CATV		Memc[CAT_AREA($1)+($2-1)*SZ_OBJECT]

define  COL_VALUE	11
define	SZ_DATE		9
define  SZ_DEC		10

# XTENSION values
define	TABLE		2
define  BINTABLE	3
define  IMAGE		4
define  OTHER		5

define	FITS_BYTE	8	# Bits in a FITS byte
define	LSBF		NO	# Least significant byte first
define	LEN_CARD	80	# Length of FITS card in characters
