# ADB.H -- Macros and constants used by adb interface

# Symbolic constant used for error code (must agree with adbinterf.h)

define	ADB_ERRCODE	401

# Symbolic constants used to select database system (must agree with stdb.h)

define	IDM_SQL		2
define	IDM_IQL		3
define	SYBASE		4

# Symbolic constants used to select error level (must agree with stdb.h)

define ADB_OK       100
define ADB_INFO     101
define ADB_STAT     102
define ADB_WARN     103
define ADB_ERR      104

# Array dimensions

define	SZ_CNAME	19	# set to agree with SDAS table column length
define	ADB_MAXCOL	256	# maximum number of columns in database table

# Possible states of program accessing database

define	ADB_CLOSED	0
define	ADB_OPENED	1
define	ADB_QUERIED	2

# Macros used to access bound variables

define	ADB_NAME	Memc[adb_namptr+($1-1)*(SZ_CNAME+1)]
define	ADB_QNAME	Memc[adb_qnamptr+($1-1)*(SZ_CNAME+1)]
define	ADB_TYPE	Memi[adb_typptr+($1-1)]
define	ADB_OFFSET	Memi[adb_offptr+($1-1)]
define	ADB_COLUMN	Memi[adb_buffer+ADB_OFFSET($1)]

# Default values used by the system

define	ADB_DEFSYSTEM	SYBASE
define	ADB_DEFSERVER	"SYBASE_1"

# How often to print a diagnostic message about number of rows written

define	ADB_MODWRITE	20

# Definition of delimeters used in parsing

define	IS_DELIM	(($1) <= ' ' || ($1) == ',')
define	NOT_DELIM	(($1) > ' ' && ($1) != ',')
