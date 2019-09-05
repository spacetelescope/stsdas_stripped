# LOG.H -- Definition of the logger structure

define	LEN_LOGSTRUCT	4
define	SZ_LOGMSG	SZ_LINE

define	LOG_WINDOW	Memi[$1]	# window descriptor
define	LOG_HIDDEN	Memi[$1+1]	# is window hidden?
define	LOG_MSGPTR	Memi[$1+2]	# pointer to message
define	LOG_RING	Memi[$1+3]	# ring the bell for errors?

define	LOG_MESSAGE	Memc[LOG_MSGPTR($1)]
