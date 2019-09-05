# Macro definitions for the task convtime taken from epoch.h

define	MJD2001		51910   	# modified JD of Jan 1, 2001, 0h 
define  MJD1201         -240277         # modified JD of Jan 1, 1201 (O.S.), 0h
define	JDMJD		2400000.5d0
define	SECPERDAY	86400.d0	

# limits of arguments
define	MAX_YEAR	5800000
define	MAX_MJD		2.1d9
 
# months
define	SZ_MONTH	3
define	SZ_DAY		3
define	MONTH		"|jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec|"

# style
define	NEWSTYLE	0	# Gregorian calendar
define	OLDSTYLE	1	# Julian calendar

# epoch lengths (MUST be integers)
define	QCENTURY	146097
define	CENTURY		36524
define	QANNUM		1461
define	ANNUM		365
