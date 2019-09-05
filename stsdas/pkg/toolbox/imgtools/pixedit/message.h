# MESSAGE.H -- Definition of the message structure and constants

define	LEN_MSG		4
define	LEN_MSGBUF	8000
define	LEN_MSGQUEUE	100

define	MSG_KIND	Memi[$1]	# kind of message
define	MSG_OBJ		Memi[$1+1]	# object receiving message
define	MSG_ARG1	Memi[$1+2]	# first message argument (optional)
define	MSG_ARG2	Memi[$1+3]	# second message argument (optional)

define	IO_KEYBOARD	0
define	IO_CURSOR	1
