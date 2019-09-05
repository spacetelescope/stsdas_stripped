# Catalog/Stars structure
define	LEN_STAR_STR	64
define	MAG_SIZE_SLOPE	Memr[($1)]		# Mag to size transformation
define	MAG_SIZE_INTERC	Memr[($1+1)]
define	LINE_SEP	Memr[($1+2)]		# Line sep in NDC
define	DEV_ASPECT	Memr[($1+3)]		# Device aspect ratio
define	WCS_NDC_X	Memr[($1+4)]		# WCS/NDC x scale
define	WCS_NDC_Y	Memr[($1+5)]		# WCS/NDC y scale
define	FAINT_PLOT_MAG	Memr[($1+6)]		# Faintest star plotted
define	BRIGHT_PLOT_MAG	Memr[($1+7)]		# Brightest star plotted
define	SYMBOL_STYLE	Memi[($1+8)]		# Object symbol style

define	NUM_CAT_VALS	Memi[($1+10)]		# Number of elements
define	CAT_RA_P	Memi[($1+11)]
define	CAT_RA_VAL	Memd[CAT_RA_P($1)]	# R.A. values
define	CAT_DEC_P	Memi[($1+12)]
define	CAT_DEC_VAL	Memd[CAT_DEC_P($1)]	# Dec. values
define	CAT_MAG_P	Memi[($1+13)]
define	CAT_MAG_VAL	Memr[CAT_MAG_P($1)]	# Mag. values
define	CAT_TITLE_P	Memi[($1+15)]
define	CAT_TITLE	Memc[CAT_TITLE_P($1)]	# Catalog title
define	CAT_ROW_P	Memi[($1+16)]
define	CAT_TBL_ROW	Memi[CAT_ROW_P($1)]	# Table row
define	CAT_NAM_P	Memi[($1+21)]
define	CAT_NAME	Memc[CAT_NAM_P($1)]	# Name string
define	CAT_NAME_SIZE	Memi[($1+22)]		# Object name size
define	NAME_COL_EX	Memi[($1+23)]		# Names exist?
define	CAT_CLS_P	Memi[($1+24)]
define	CAT_CLASS	Memi[CAT_CLS_P($1)]	# Class codes
define	CLASS_COL_EX	Memi[($1+25)]		# Class column exists?
define	VALID_OBJ_P	Memi[($1+26)]
define	VALID_OBJECT	Memb[VALID_OBJ_P($1)]	# Valid object?

define	NULL_COL	Memb[(($1)+(($2)-1)*($3))]

define	COLOR_INDEX	Memi[($1+29)]		# Color Index

define	MIN_RA		Memd[P2D($1+30)]	# Catalog coordinate limits
define	MIN_DEC		Memd[P2D($1+32)]
define	MAX_RA		Memd[P2D($1+34)]
define	MAX_DEC		Memd[P2D($1+36)]
define	FAINT_CAT_MAG	Memr[($1+38)]		# Faintest star in memory
define	BRIGHT_CAT_MAG	Memr[($1+39)]		# Brightest star in memory

define	CEN_MARK_SYMB	Memi[($1+41)]		# Central marker style
define	CEN_MARK_SIZE	Memr[($1+42)]		# Marker size (NDC)
define	CONN_STYLE	Memi[($1+43)]		# Connection style

# Plate constants structure
define	LEN_PL_CNST	32
define	SIN_A		Memd[P2D($1)]		# sin(a), a=RA at plate center
define	COS_A		Memd[P2D($1+2)]		# cos(a)
define	SIN_D		Memd[P2D($1+4)]		# sin(d), d=Dec at plate center
define	COS_D		Memd[P2D($1+6)]		# cos(d)
define	COSA_SIND	Memd[P2D($1+8)]		# cos(a)*sin(d)
define	SINA_SIND	Memd[P2D($1+10)]	# sin(a)*sin(d)
define	COSA_COSD	Memd[P2D($1+12)]	# cos(a)*cos(d)
define	SINA_COSD	Memd[P2D($1+14)]	# sin(a)*cos(d)
define	CEN_RA		Memd[P2D($1+16)]	# RA of plate center (radians)
define	CEN_DEC		Memd[P2D($1+18)]	# Dec of plate center (radians)
define	PLATE_SCALE	Memd[P2D($1+20)]	# Plate scale in rad/mm

define	MIRROR_FLIP	Memi[($1+24)]		# Flip?

# Units conversion macros
define	RADTOST		(240*RADTODEG($1))	# Radians to seconds of time
define	RADTOSA		(3600*RADTODEG($1))	# Radians to seconds of arc
define	STTORAD		(DEGTORAD(($1)/240))	# Seconds of time to radians
define	SATORAD		(DEGTORAD(($1)/3600))	# Seconds of arc to radians
define	RADTOHRS	(RADTODEG(($1)/15))	# Radians to hours
define	HRSTORAD	(DEGTORAD(15*($1)))	# Hours to radians

define	STPERDAY	86400			# Seconds per day
define	MAX_WIDTH	PI/4

# WCS numbers
define	NDC_WCS		0
define	WCS_WCS		1
define	SEC_WCS		2
define	MM_F_WCS	3
define	MM_R_WCS	4
define	STAR_KEY_WCS	13
define	LEGEND_WCS	14
define	MENU_WCS	15

define	RA_INCR		25
define	RA_NUM_TRY	6
define	DEC_NUM_TRY	4
define	ALMOST_POLE	(DEGTORAD(86))
define	EDGE_FACTOR	60
define	TEXT_LINES	13

define	SUPER_SCRIPT	1
define	NORMAL_SCRIPT	0
define	SUB_SCRIPT	-1
define	SS_FACTOR	.4

define	NUM_STARS	9
define	MIN_SIZE	.0001
define	MAX_SIZE	.03
define	OVERLAP		1.0
define	RASTER_WIDTH	1.5

define	NUM_COLS	5
define	RA_COL		1
define	DEC_COL		2
define	MAG_COL		3
define	NAM_COL		4
define	CLS_COL		5
define	ROW_COL		6

define	NONE_STAR	0
define	FILLED_STAR	1
define	OPEN_STAR	2
define	ERASE_STAR	3
define	EFILLED_STAR	4
define	PLUS_STAR	5
define	CROSS_STAR	6
define	SQUARE_STAR	7
define	DIAMOND_STAR	8
define	CIRCLE_STAR	9

define	CLEAR_MARGIN	.0025
