# IMAGE.H -- Description of image structure and constants

define	LEN_IMGSTRUCT	28		# length of image object structure
define	IMG_MAXSTR	PIXWIDTH	# maximum length of string

define	IMG_WINDOW	Memi[$1]	# window descriptor
define	IMG_HIDDEN	Memi[$1+1]	# is window hidden?
define	IMG_DESCRIP	Memi[$1+2]	# image descriptor
define	IMG_RDONLY	Memi[$1+3]	# is image read only?
define	IMG_INPLACE	Memi[$1+4]	# edit image in place?
define	IMG_UPDFILE	Memi[$1+5]	# has file been modified?
define	IMG_UPDBUF	Memi[$1+6]	# has current buffer been modified?
define	IMG_UPDPIX	Memi[$1+7]	# has current pixel  been modified?
define	IMG_TYPE	Memi[$1+8]	# data type used to read image
define	IMG_NROW	Memi[$1+9]	# number of rows displayed
define	IMG_NCOL	Memi[$1+10]	# number of columns displayed
define	IMG_CWIDTH	Memi[$1+11]	# column width
define	IMG_LOROW	Memi[$1+12]	# lowest row displayed
define	IMG_HIROW	Memi[$1+13]	# highest row displayed
define	IMG_LOCOL	Memi[$1+14]	# lowest column displayed
define	IMG_HICOL	Memi[$1+15]	# highest column displayed
define	IMG_CURROW	Memi[$1+16]	# index of current row
define	IMG_CURCOL	Memi[$1+17]	# index of current column
define	IMG_CURPLANE	Memi[$1+18]	# index of current plane
define	IMG_STRIDX	Memi[$1+19]	# index of current field
define	IMG_STRLEN	Memi[$1+20]	# length of current field
define	IMG_PREV	Memi[$1+21]	# ptr to previous image which is same
define	IMG_NEXT	Memi[$1+22]	# ptr to next image which is same
define	IMG_NAMPTR	Memi[$1+23]	# ptr to original image name
define	IMG_STRPTR	Memi[$1+24]	# ptr to string holding current field
define	IMG_DELPTR	Memi[$1+25]	# ptr to deletion buffer
define	IMG_FMTPTR	Memi[$1+26]	# ptr to display format
define	IMG_BUFPTR	Memi[$1+27]	# ptr to data buffer

define	IMG_NAME	Memc[IMG_NAMPTR($1)]
define	IMG_STR		Memc[IMG_STRPTR($1)+$2]
define	IMG_CURCHAR	Memc[IMG_STRPTR($1)+IMG_STRIDX($1)]
define	IMG_NXTCHAR	Memc[IMG_STRPTR($1)+IMG_STRIDX($1)+1]
define	IMG_DELBUF	Memc[IMG_DELPTR($1)+$2]
define	IMG_FORMAT	Memc[IMG_FMTPTR($1)]

define	TITLE_HEIGHT	1		# height of image window title

define	MV_UP		1		# Relative motion constants
define	MV_DOWN		2
define	MV_RIGHT	3
define	MV_LEFT		4

define	POS_CENTER	0		# Relative motion positions
define	POS_LEFT	1
define	POS_RIGHT	2

