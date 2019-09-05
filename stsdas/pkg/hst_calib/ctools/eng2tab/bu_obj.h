#---------------------------------------------------------------------------
.help bu_obj.h May94 source
.ih
NAME
bu_obj.h -- Definition of the bit unpack object.
.endhelp
#---------------------------------------------------------------------------

# Structure of the object.
define	BU_N_ITEMS		Memi[$1]
define	BU_DB       		Memi[$1+1]
define	BU_VALUE_ARRAY_PTR     	Memi[$1+2]
define	BU_VALUE_ARRAY	       	Memi[BU_VALUE_ARRAY_PTR($1)+$2-1]
define	BU_VALUE	       	Meml[BU_VALUE_ARRAY($1,$2)+$3-1]
define	BU_N_VALUES	       	Memi[$1+3]
define	BU_MAX_VALUES	       	Memi[$1+4]
define	BU_FNAME_PTR	       	Memi[$1+5]
define	BU_FNAME	       	Memc[BU_FNAME_PTR($1)+(($2-1)*(SZ_PATHNAME+1))]
define	BU_IS_NULL_PTR		Memi[$1+6]
define	BU_IS_NULL		Memi[BU_IS_NULL_PTR($1)+$2-1]
define	BU_LIST_PTR		Memi[$1+7]
define	BU_LIST			Memc[BU_LIST_PTR($1)]
define	BU_SIZE		       	8

# Constants
define	BU_EXTRACT		"extract"
define	BU_FORMAT		"format"
define	BU_GROW			100
define	BU_UNITS		"units"
define	BU_SZ_EXPR		SZ_COMMAND

# Function dictionary.
define	BU_FUNCTIONS	",and,shift,switch,word_find,lin"
define	BU_FUNC_AND		1
define	BU_FUNC_SHIFT		2
define	BU_FUNC_SWITCH		3
define	BU_FUNC_WRDFND		4
define	BU_FUNC_LIN		5

# Variable dictionary
define	BU_VARIABLES	",d,v"
define	BU_VAR_D		1
define	BU_VAR_V		2
#---------------------------------------------------------------------------
# End of bu_obj.h
#---------------------------------------------------------------------------
