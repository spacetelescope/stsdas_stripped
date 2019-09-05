define	MAX_ERROR	1.d0	# max range (degrees) without printing warning

define	UNKNOWN		0	# can't determine parity
define	NORMAL		1	# same parity as the sky
define	REVERSED	2	# mirror image of the sky

define	SZ_KEYVAL	19	# size of a short string (e.g keyword value)

# yyyymmdd when images were flipped around Y axis and theta values changed.
define	FLIP_DATE	19910125

# Values of theta:

define	OLD_THETA_48	154.9d0
define	OLD_THETA_96	 33.23d0

define	NEW_THETA_48	159.4226d0	# starting FLIP_DATE
define	NEW_THETA_96	 34.56d0

define	THETA_48_COSTAR 159.4226d0	# (new value not measured yet)
define	THETA_96_COSTAR  33.59d0	# starting COSTAR_DATE

# yyyymmdd when keywords changed.
define	KEYNAME_DATE	19911112

# yyyymmdd when coordinate values were finally correct.
define	OK_DATE		19920527

# yyyymmdd when COSTAR was deployed for FOC.
define	COSTAR_DATE	19931226
