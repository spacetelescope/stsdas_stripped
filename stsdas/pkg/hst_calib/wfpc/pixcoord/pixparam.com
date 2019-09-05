# PIXPARAM.COM -- Common block used to hold previous parameter value

char	oldname[PAR_MAXSTR]
char	oldvalue[PAR_MAXSTR]

common	/param/ oldname, oldvalue

