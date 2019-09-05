# OBJECT.H -- Definition of object descriptor

define	LEN_OBJSTRUCT	6		# length of object structure

define	OBJ_KIND	Memi[$1]	# kind of object
define	OBJ_DESCRIP	Memi[$1+1]	# distinct part of object description
define	OBJ_FUNC	Memi[$1+2]	# function that receives messages
define	OBJ_PARENT	Memi[$1+3]	# parent object
define	OBJ_CHILD	Memi[$1+4]	# head of child object list
define	OBJ_NEXT	Memi[$1+5]	# sibling object

define	OBJ_MAXDEPTH	20		# maximum depth of object structure

define	LEN_OSSTRUCT	OBJ_MAXDEPTH+1	# size of object search stack

define	OS_TOP		Memi[$1]	# index to top of search stack
define	OS_STACK	Memi[$1+$2]	# stack of pending objects
