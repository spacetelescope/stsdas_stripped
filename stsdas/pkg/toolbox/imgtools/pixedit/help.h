# HELP.H -- Definition of the help window structure

define	LEN_HLPSTRUCT	12	
define	HLP_SIZEBUF	1000		# size of initial help text buffer
define	HLP_SIZEARY	25		# size of initial row array

define	HLP_WINDOW	Memi[$1]	# window descriptor
define	HLP_HIDDEN	Memi[$1+1]	# is window hidden?
define	HLP_LOROW	Memi[$1+2]	# lowest displayed row in help text
define	HLP_HIROW	Memi[$1+3]	# highest displayed row in help text
define	HLP_CURROW	Memi[$1+4]	# current row in help text
define	HLP_CURCOL	Memi[$1+5]	# current column in help text
define	HLP_MAXBUF	Memi[$1+6]	# size of help text buffer
define	HLP_NXTBUF	Memi[$1+7]	# next free location in buffer
define	HLP_MAXROW	Memi[$1+8]	# maximum number of text rows
define	HLP_NXTROW	Memi[$1+9]	# next free text row
define	HLP_BUFPTR	Memi[$1+10]	# pointer to text buffer
define	HLP_ROWARY	Memi[$1+11]	# pointer to row array

define	HLP_BUFFER	Memc[HLP_BUFPTR($1)+HLP_NXTBUF($1)]
define	HLP_ROWPTR	Memi[HLP_ROWARY($1)+$2]

define	HLP_FILE	"tools$pixedit/pixedit.key"
