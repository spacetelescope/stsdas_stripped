#---------------------------------------------------------------------------
.help shift_table.h Feb93 source
.ih
NAME
shift_table.h -- Definition of the poffsets' output shift table.
.endhelp
#---------------------------------------------------------------------------

# Define the columns needed.
define  OT_FNAME        1               # Filename column.
define  OT_GROUP        2               # Group column.
define  OT_SHIFT        3               # Shift column.
define  OT_WAVE         4               # Wavelength file.
define  OT_WGROUP       5               # Group of wavelenght.
define  OT_WSHIFT       6               # Shifts for wavelengths.
define  OT_N_COLUMNS    6               # Number of columns.
define  OT_N_COL_NOWAVE 3               # Number of columns with no waves.

# Define the memory structure that will hold column pointers, etc.
define  OT_TP           Memi[$1]        # Table descriptor.
define  OT_C_PTR        Memi[$1+1]      # Array of column descriptors.
define  OT_C            Memi[OT_C_PTR($1)+$2-1]
define  OT_LAST         Memi[$1+2]      # Current last row of table.
define  OT_SAVEWAVE     Memi[$1+3]      # YES if saving wavelength info.
define  SZ_OT_SZ        4               # Size of memory.
#---------------------------------------------------------------------------
# End of shift_table.h
#---------------------------------------------------------------------------
