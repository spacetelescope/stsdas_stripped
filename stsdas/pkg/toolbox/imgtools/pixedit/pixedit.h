# PIXEDIT.H -- Constants used by subroutines of pixedit

# Array sizes

define	PIXWIDTH	31		# maximum length of pixel string

# Object classes, used when creating objects

define	O_TERMINAL	1		# object is a terminal
define	O_WINDOW	2		# object is a (generic) window
define	O_IMAGE		3		# object is an image
define	O_LOG		4		# object is a log window
define	O_CMD		5		# object is a command window
define	O_HELP		6		# object is a help window

# Macros for testing location of windows

define	IS_TOPWIN	(OBJ_KIND($1) == O_IMAGE || OBJ_KIND($1) == O_HELP)
define	IS_BOTWIN	(OBJ_KIND($1) == O_LOG || OBJ_KIND($1) == O_CMD)

# Message codes

define	M_CREATE	1		# Create object
define	M_DESTROY	2		# Destroy object

define	T_QUIT		101		# Quit editor
define	T_REFOCUS	102		# Change the window which has focus
define	T_ADDWIN	103		# Add a window to the terminal
define	T_DELWIN	104		# Delete a window from the terminal
define	T_FRONT		105		# Move window to front
define	T_USEKEY	106		# Get input from keyboard
define	T_USECUR	107		# Get input from image cursor

define	W_RDKEY		201		# Read keyboard
define	W_RDCUR		202		# Read cursor
define	W_RESIZE	203		# Resize window
define	W_REDRAW	204		# Redraw window
define	W_HIDDEN	205		# Mark window as hidden or shown
define	W_FOCUS		206		# Move cursor into window

define	I_READ		301		# read an image file
define	I_WRITE		302		# write an image file
define	I_MOVE		303		# move cursor in image window
define	I_UPDATE	304		# update a pixel value in window
define	I_SET		305		# set the flags in the descriptor
define	I_SETFMT	306		# set the display format

define	L_ERROR		401		# error message to logger
define	L_WARN		402		# warning message to logger
define	L_INFO		403		# informational message to logger
define	L_CLEAR		404		# clear logger window

# Info request codes

define	T_TERM		1101		# get terminal's object
define	T_FOCUS		1102		# get window with keyboard focus
define	T_SELECT	1103		# get window currently selected
define	T_LOG		1104		# get log window
define	T_HELP		1105		# get help window
define	T_NEXT		1106		# get next window containing image
define	T_BOTTOM	1107		# get the bottom window

define	W_TOPROW	1201		# get top row of window
define	W_HEIGHT	1202		# get height of window

define	I_NAME		1301		# get image name
define	I_RDONLY	1302		# get read only flag
define	I_INPLACE	1303		# get in place flag
define	I_UPFILE	1304		# get image update flag
define	I_NAXIS		1305		# get number of axes
define	I_NAXIS1	1306		# get length of first axis
define	I_NAXIS2	1307		# get length of second axis
define	I_NAXIS3	1308		# get length of third axis
define	I_AXIS1		1309		# get current row
define	I_AXIS2		1310		# get current column
define	I_AXIS3		1311		# get current plane

# Command codes

define 	CMD_CURSOR	-3	## hide cursor command until bug is fixed
define	CMD_FINISH	-2
define	CMD_QWRITE	-1
define	CMD_ANY		0
define	CMD_EXIT	1
define	CMD_FORMAT	2
define	CMD_GOTO	3
define	CMD_HELP	4
define	CMD_QUIT	5

define	CMD_LIST	"|exit|format|goto|help|quit|"

# Structures used in messages

# Create a new image structure

define	LEN_INEWSTRUCT	3

define	INEW_FNAME	Memi[$1]	# Image name
define	INEW_RDONLY	Memi[$1+1]	# Is image read only?
define	INEW_INPLACE	Memi[$1+2]	# Is image edited in place?

# Create a new log structure

define	LEN_LNEWSTRUCT	1

define	LNEW_SILENT	Memi[$1]	# no warning bell flag

# Create a new command structure

define	LEN_CNEWSTRUCT	2

define	CNEW_CODE	Memi[$1]	# Command code
define	CNEW_STRPTR	Memi[$1+1]	# Command string pointer

# Image position structure

define	LEN_IPOSSTRUCT	4

define	IPOS_INDEX	Memi[$1]	# index to character in pixel
define	IPOS_ROW	Memi[$1+1]	# pixel row
define	IPOS_COL	Memi[$1+2]	# pixel column
define	IPOS_PLANE	Memi[$1+3]	# pixel plane

# Image cursor structure

define	LEN_CURSSTRUCT	3

define	CURS_KEY	Memi[$1]	# key pressed during cursor read
define	CURS_ROW	Memi[$1+1]	# cursor row when key pressed
define	CURS_COL	Memi[$1+2]	# cursor column when key pressed

