# COMMAND.H -- Definition of the command structure

define	LEN_CMDSTRUCT	10
define	CMD_MAXSTR	SZ_LINE

define	CMD_WINDOW	Memi[$1]	# window descriptor
define	CMD_HIDDEN	Memi[$1+1]	# is window hidden?
define	CMD_FIELD	Memi[$1+2]	# current field
define	CMD_FUNC	Memi[$1+3]	# validation function
define	CMD_STRIDX	Memi[$1+4]	# current character in current field
define	CMD_STRLEN	Memi[$1+5]	# current length of current field
define	CMD_STRPTR	Memi[$1+6]	# pointer to current field
define	CMD_BUFPTR	Memi[$1+7]	# pointer to deletion buffer
define	CMD_TTLPTR	Memi[$1+8]	# pointer to title buffer
define	CMD_CMDPTR	Memi[$1+9]	# pointer to command buffer

define	CMD_STR		Memc[CMD_STRPTR($1)+$2]
define	CMD_CURCHAR	Memc[CMD_STRPTR($1)+CMD_STRIDX($1)]
define	CMD_NXTCHAR	Memc[CMD_STRPTR($1)+CMD_STRIDX($1)+1]
define	CMD_CHCOL	(CMD_STRIDX($1)+CMD_STRCOL)

define	CMD_BUF		Memc[CMD_BUFPTR($1)+$2]
define	CMD_TITLE	Memc[CMD_TTLPTR($1)]
define	CMD_COMMAND	Memc[CMD_CMDPTR($1)]

define	CMD_TTLROW	1
define	CMD_TTLCOL	1
define	CMD_STRROW	2
define	CMD_STRCOL	1

