define	TVS_DEFAULT	0	# determine direction from name with node
define	VAX_SUN		1	# convert from VAX format to Sun format
define	SUN_VAX		2	# Sun to VAX
define	VAX_DEC		3	# VAX to DecStation
define	DEC_VAX		4	# DecStation to VAX
define	DEC_SUN		5	# DecStation to Sun
define	SUN_DEC		6	# Sun to DecStation

# INDEF values on a Sun but expressed on a VAX.
# These are integers (base 10) equivalenced to the floating point values.
define	TVS_SUN_INDEFD1 -1206395321	# b817de47
define	TVS_SUN_INDEFD2   459888451	# 1b695743
define	TVS_SUN_INDEFR  -1027739522	# c2bdf07e
# define	TVS_SUN_INDEFI     16777344	# 1000080
# define	TVS_SUN_INDEFS          384	# 180

# INDEF values on a VAX but expressed on a Sun.
define	TVS_VAX_INDEFD1  -260062531	# f07fc2bd
define	TVS_VAX_INDEFD2 -1155867832	# bb1adb48
define	TVS_VAX_INDEFR   -260062531	# f07fc2bd
# define	TVS_VAX_INDEFI          128	# 80 (obsolete)
# define	TVS_VAX_INDEFS          128	# 80 (obsolete)

# NOTE:  these must agree with tbtables.h
define	LEN_SIZINFO		12		# unit = SZ_INT
define	SZ_SIZINFO		(LEN_SIZINFO * SZ_INT)
define	LEN_COLSTRUCT		16		# unit = SZ_STRUCT
define	SZ_COLSTRUCT		(LEN_COLSTRUCT * SZ_STRUCT)
define	COL_LEN			Memi[$1+2]	# chars for data element
