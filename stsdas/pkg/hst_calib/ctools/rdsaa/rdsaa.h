#---------------------------------------------------------------------------
.help rdsaa.h Jun92 tools
.ih
NAME
rdsaa.h -- Include file for the rdsaa code.
.endhelp
#---------------------------------------------------------------------------

# Define the various SAA record types.  Following are the common elements.
# After that are fields specific to each form of record.
define  SAA_RECORD_SIZE 17

define  RECORD_TYPE     Memi[$1]                # Type of current record.
define  RECORD          Memi[$1+2]              # Current record number

# Fields specific to the FD type record.
define  FILE_NUMBER     Memi[$1+3]              # The File number in the PDB
define  FILE_TYPE_PTR   Memi[$1+4]              # File type in the PDB
define  FILE_TYPE       Memc[FILE_TYPE_PTR($1)]
define  N_RECORDS       Memi[$1+5]              # Total # of records in file.
define  CREATE_DATE_PTR Memi[$1+6]              # Create date: MMYYDD
define  CREATE_DATE     Memc[CREATE_DATE_PTR($1)]
define  DESCR_PTR       Memi[$1+7]              # Descriptive text.
define  DESCR           Memc[DESCR_PTR($1)]
define  CONTACT_PTR     Memi[$1+8]              # Person to contact.
define  CONTACT         Memc[CONTACT_PTR($1)]
define  TNUMBER_PTR     Memi[$1+9]              # Telephone number of contact.
define  TNUMBER         Memc[TNUMBER_PTR($1)]

# Fields specific to the GC type record.
define  COMMENT_PTR     Memi[$1+10]             # Comment.
define  COMMENT         Memc[COMMENT_PTR($1)]

# Fields specific to the VD type record.
define  MODEL_ID        Memi[$1+11]             # Model ID number.
define  VERTEX          Memi[$1+12]             # Vertex number.
define  LATITUDE        Memr[$1+13]             # Latitude.
define  LONGITUDE       Memr[$1+14]             # Longitude.
define  ACTIVE_FLAG     Memi[$1+15]             # Active flage: YES or NO.
define  FLUX            Memr[$1+16]             # Flux (apprently not used)

# Define the various types of records there can be in the PDB file.
define  TYPE_DICT       "|FD|GC|VD"
define  FD              1
define  GC              2
define  VD              3
define  N_TYPES         3               # How many record types there are.
define  SZ_RT           2               # Two characters define the record type

# Minimum length of a line from a PDB file.
define  SZ_PDB_RECORD   80

# Define the maximum number of models there will be.
define  MAX_MODELS      110

#---------------------------------------------------------------------------
# End of rdsaa.h
#---------------------------------------------------------------------------
