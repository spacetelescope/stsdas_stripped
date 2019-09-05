define	SECPERDAY	86400.D0	# number of sec in a day
define	MINPERDAY	1440.D0		# number of min in a day
define	HRPERDAY	24.D0		# number of hours in a day

define	LYPERPC		3.261633D0	# light years per parsec
define	KMPERPC		3.085678D13	# kilometers per parsec
define	AUPERPC		206264.8062470964D0	# how many AU in one parsec
define	KMPERAU		KMPERPC/AUPERPC
define	CLIGHT 		499.00479D0	# light travel time (in sec) of 1 AU

define	EARTH_EPHEMERIS	1
define	OBS_EPHEMERIS	2

define	JD_TO_MJD	(2400000.5d0)	# subtract from JD to get MJD
