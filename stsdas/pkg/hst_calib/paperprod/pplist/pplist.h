define	MAX_OBS		1000	# maximum number of observations
define	SZ_EXT		20	# file extension size
define	SZ_FEXT		4
define	BOTTOM		30.	# number of rows in a page

define	GOOD		0
define	UNKNOWN		1
define	BAD		2
define	NA		4

# fields in the target list
define	SZ_VISIT	2
define	SZ_NVISIT	3
define	SZ_TARG		30	
define	SZ_DESC		35	

# fields in the observation list, common fields.
define	SZ_LINENUM	8
define	SZ_ROOT		9
define	MAX_TARG	15
define	SZ_DATEOBS	8
define	SZ_EXPT		8
define	MAX_OPTION	60
define	SZ_MTFLAG	2

# ACS specific
define  SZ_JDET     4
define  SZ_JMODE    8
define  SZ_JAPER    16
define  SZ_JFILT    7
define  JLEN        122.


# FOS specific
define	SZ_YMODE	10	
define	SZ_YDET		5
define	SZ_YAPER	23
define	MAX_APER	16
define	SZ_YFILT	3

# GHRS specific
define	SZ_ZCONFIG	5	
define	SZ_ZMODE	10	
define	SZ_ZAPER	5
define	SZ_ZFILT	9
define	SZ_ZWAVE	5

# STIS specific
define	SZ_OMODE	8	
define	SZ_ODET		8
define	SZ_OAPER	16
define	SZ_OFILT	6
define	SZ_OCWAVE	5
define	SZ_OOPTION	180

# NICMOS specific
define	SZ_NMODE	10
define	SZ_NFILT	10
define	SZ_NPATTERN	16
define	MAX_PATTERN	11

# WFPC2 specific
define	SZ_UMODE	5
define	SZ_UAPER	8
define	SZ_UFILT	8
define	MAX_FILT	11
define	SZ_USERIAL	3
define	SZ_UGAIN	4
define	SZ_USERGAIN	8
define	SZ_UFILTNAM	17
define	MAX_UGRPS	4
define	SZ_PED		40

#FOC specific
define	SZ_XFILT	6
define	SZ_XFILTNAM	13
define	XIMG_BORD	20
define	XSECT_MIN	20
define	XSECT_MAX	1000
