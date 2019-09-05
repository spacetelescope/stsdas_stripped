define	MJD1980		44239    	# modified JD of Jan 1, 1980, 0h
define	MJD850100	46065    	# modified JD of Jan 0, 1985, 0h
define	MJD850101	46066    	# modified JD of Jan 1, 1985, 0h
define	MJD2001		51910   	# modified JD of Jan 1, 2001, 0h 
define	MJD1201		(-240277)	# modified JD of Jan 1, 1201 (O.S.), 0h
define	JDMJD		2400000.5d0
define	SECPERDAY	86400.d0	

# limits of arguments
define	MAX_YEAR	5800000
define	MAX_MJD		2.1d9

# months
define	SZ_MONTH	3
define	SZ_DAY		3
define	MONTH		"|jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec|"

# qualifiers:
define	BLANKSPACE	0
define	MJD		1
define	JD		2
define	CDBS		3
define	DMF		4
define	JULIAN		5
define	BESSEL		6
define	GEISUT		7
define	SMS		8
define	EPCHTIME	9
define	TIMEFFEC	9
define	OBSSTRTT	10
define	ENG		11

# allowed words
define	WORDS		"|now|today|launch|deploy|"
define	NOW		1
define	TODAY		2
define	LAUNCH		3
define	DEPLOY		4
define	TLAUNCH		48005.5236
define	TDEPLOY		48006.8181

# style
define	NEWSTYLE	0	# Gregorian calendar
define	OLDSTYLE	1	# Julian calendar

# epoch lengths (MUST be integers)
define	QCENTURY	146097
define	CENTURY		36524
define	QANNUM		1461
define	ANNUM		365

# time zone definition
define	EST		5.D0
define	EDT		4.D0
define	CST		6.D0
define	CDT		5.D0
define	MST		7.D0
define	MDT		6.D0
define	PST		8.D0
define	PDT		7.D0
define	GMT		0.D0
