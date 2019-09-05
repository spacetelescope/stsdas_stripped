#---------------------------------------------------------------------------
.help groupmod.h Jul92 tools
.ih
NAME
groupmod.h -- Include files for the groupmod source.
.endhelp
#---------------------------------------------------------------------------

# Define the size of a keyword name.
define  SZ_KEYNAME      8

# Define maximum length of character strings.
define  MAXPLEN         SZ_LINE

# Define where the temporary files go.
define  TMP_ROOT        "tmp$groupmod"

# Define the types of operations.  Note that the positions of the operations
# in the string match their corresponding values.
define  OP_DICT         "|add|delete"
define  OP_ADD          1
define  OP_DEL          2

# Define the acceptable data types.  Note that this does not include all
# SPP types.  This business of adding groups to existing images is sensitive
# and is only garaunteed (?) for "normal" types.
define  TYPE_DICT       "|LONG|INTEGER|REAL|DOUBLE|CHARACTER|BOOLEAN"
define  GRM_LONG        1
define  GRM_INT         2
define  GRM_REAL        3
define  GRM_DOUBLE      4
define  GRM_CHAR        5
define  GRM_BOOL        6

# Define the data structure about the keyword table.
define  KW_OP           Memi[$1]
define  KW_N_KEYWORDS   Memi[$1+1]
define  KW_NAME_PTR     Memi[$1+2]
define  KW_NAME         Memc[KW_NAME_PTR($1)+(($2-1)*(SZ_KEYNAME+1))]
define  KW_DTYPE_PTR    Memi[$1+3]
define  KW_DTYPE        Memi[KW_DTYPE_PTR($1)+$2-1]
define  KW_PLEN_PTR     Memi[$1+4]
define  KW_PLEN         Memi[KW_PLEN_PTR($1)+$2-1]
define  KW_INIT_PTR     Memi[$1+5]
define  KW_INIT         Memc[KW_INIT_PTR($1)+(($2-1)*(MAXPLEN+1))]
define  KW_COMM_PTR     Memi[$1+6]
define  KW_COMM         Memc[KW_COMM_PTR($1)+(($2-1)*(MAXPLEN+1))]
define  SZ_KW_STRUCT    7

#---------------------------------------------------------------------------
# End of groupmod.h
#---------------------------------------------------------------------------
